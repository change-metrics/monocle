# Monocle.
# Copyright (C) 2019-2021 Monocle authors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import os
import socket
import sys
import time
import yaml

from typing import Dict, List, Optional

from flask import Flask
from flask import abort
from flask import jsonify
from flask import make_response
from flask import request
from flask import redirect
from flask import session
from flask_cors import CORS
from flask_caching import Cache

# https://docs.authlib.org/en/latest/client/frameworks.html#frameworks-clients
# https://docs.authlib.org/en/latest/client/flask.html#flask-client
from authlib.integrations.flask_client import OAuth

from monocle import utils
from monocle.db.db import CHANGE_PREFIX
from monocle.db.db import ELmonocleDB
from monocle.db.db import InvalidIndexError
from monocle.tracker_data import extract_data, InputTrackerData
from monocle import config


CACHE_TIMEOUT = 300  # 5 mn cache
INPUT_TRACKER_DATA_LIMIT = 500

cache = Cache(config={"CACHE_TYPE": "simple"})
app = Flask(__name__)
cache.init_app(app)

app.secret_key = os.urandom(16)
CORS(
    app,
    resources={r"/api/0/*": {"origins": os.getenv("ALLOW_ORIGIN", "*")}},
    supports_credentials=True,
)
oauth = OAuth(app)

oauth.register(
    name="github",
    client_id=os.getenv("CLIENT_ID"),
    client_secret=os.getenv("CLIENT_SECRET"),
    access_token_url="https://github.com/login/oauth/access_token",
    access_token_params=None,
    authorize_url="https://github.com/login/oauth/authorize",
    authorize_params=None,
    api_base_url="https://api.github.com/",
    client_kwargs={"scope": "user:email"},
)

indexes_acl: Dict[str, List[config.Username]] = {}
project_defs: Dict[str, List[config.ProjectDefinition]] = {}


config_path = os.getenv("CONFIG", None)
if not config_path:
    print("CONFIG env is missing.", file=sys.stderr)
else:
    if not os.path.isfile(config_path):
        print("Unable to access %s." % config_path, file=sys.stderr)
        sys.exit(1)
    else:
        rawconfig = yaml.safe_load(open(config_path))
        globals()["indexes_acl"] = config.build_index_acl(rawconfig)
        globals()["project_defs"] = config.build_project_definitions(rawconfig)
        globals()["indexes_amend_api_key"] = config.build_index_amend_api_key(rawconfig)


@app.route("/api/0/health", methods=["GET"])
def health():
    data = {
        "hostname": socket.gethostname(),
        "status": "success",
        "timestamp": time.time(),
    }
    return jsonify(data)


@app.route("/api/0/login", methods=["GET"])
def login():
    github = oauth.create_client("github")
    redirect_uri = os.getenv("REDIRECT_URL")
    return github.authorize_redirect(redirect_uri)


@app.route("/api/0/authorize", methods=["GET"])
def authorize():
    github = oauth.create_client("github")
    # token = github.authorize_access_token()
    github.authorize_access_token()
    resp = github.get("user")
    profile = resp.json()
    # do something with the token and profile
    session["username"] = profile.get("login")
    # return jsonify(profile)
    return redirect(os.getenv("WEB_URL"))


@app.route("/api/0/whoami", methods=["GET"])
def whoami():
    if not os.getenv("CLIENT_ID"):
        return "Authentication not configured", 503
    username = session.get("username") or request.headers.get("Remote-User")
    return jsonify(username)


def _get_index(request):
    if not request.args.get("index"):
        abort(make_response(jsonify(errors=["No index provided"]), 404))
    return request.args.get("index")


@app.route("/api/0/projects", methods=["GET"])
def get_cfg_project_definition():
    index = _get_index(request)
    projects = project_defs[index] if index in project_defs else []
    if not projects:
        return (
            "There are no project definion set in " "config file for index %s" % index,
            404,
        )
    return jsonify(projects)


@app.route("/api/0/query/<name>", methods=["GET"])
def query(name):

    index = _get_index(request)

    if not config.is_public_index(indexes_acl, index):
        user = session.get("username") or request.headers.get("Remote-User")
        if user:
            if user not in config.get_authorized_users(indexes_acl, index):
                return "Unauthorized to access index %s" % index, 403
        else:
            return "Unauthorized to access index %s" % index, 403
    repository_fullname = request.args.get("repository")
    try:
        ret = do_query(index, repository_fullname, request.args, name)
    except Exception:
        app.logger.exception(
            "Unable to process query %s (params: %s)"
            % (name, list(request.args.items()))
        )
        return (
            (
                "The API server was unable to process the query."
                " Please retry after or modify the filter parameters."
            ),
            500,
        )
    return ret


def create_db_connection(index: Optional[str]) -> ELmonocleDB:
    return ELmonocleDB(
        elastic_conn=os.getenv("ELASTIC_CONN", "localhost:9200"),
        index=index,
        prefix=CHANGE_PREFIX,
        create=False,
        user=os.getenv("ELASTIC_USER", None),
        password=os.getenv("ELASTIC_PASSWORD", None),
        use_ssl=os.getenv("ELASTIC_USE_SSL", None),
        verify_certs=os.getenv("ELASTIC_INSECURE", None),
        ssl_show_warn=os.getenv("ELASTIC_SSL_SHOW_WARN", None),
    )


@cache.memoize(timeout=CACHE_TIMEOUT)
def do_query(index, repository_fullname, args, name):
    params = utils.set_params(args)
    db = create_db_connection(index)
    try:
        result = db.run_named_query(name, repository_fullname, params)
    except InvalidIndexError:
        return "Invalid index: %s" % request.args.get("index"), 404
    return jsonify(result)


@app.route("/api/0/indices", methods=["GET"])
def indices():
    db = create_db_connection(None)
    _indices = db.get_indices()
    indices = []
    for indice in _indices:
        if config.is_public_index(indexes_acl, indice):
            indices.append(indice)
        else:
            user = session.get("username")
            if user:
                if user in config.get_authorized_users(indexes_acl, indice):
                    indices.append(indice)
    return jsonify(indices)


@app.route("/api/0/amend/tracker_data", methods=["POST"])
def tracker_data():
    if not request.args.get("index"):
        abort(make_response(jsonify(errors=["No index provided"]), 404))
    index = request.args.get("index")
    # Check authorization
    if not request.args.get("apikey"):
        return "No API Key provided in the request", 400
    if (
        index not in globals()["indexes_amend_api_key"]
        or request.args.get("apikey") != globals()["indexes_amend_api_key"][index]
    ):
        return "Not authorized", 403
    # Input data validation
    if not request.is_json:
        return "Missing content-type application/json", 400
    json_data = request.get_json()
    if not isinstance(json_data, list):
        return "Input data is not a List", 400
    if len(json_data) > INPUT_TRACKER_DATA_LIMIT:
        return "Input data List over limit (%s items)" % (INPUT_TRACKER_DATA_LIMIT), 400
    try:
        extracted_data = extract_data(json_data)
    except Exception as exc:
        return "Unable to extract input data due to: %s" % exc, 400
    # Find changes EL ids based on change_ids
    change_ids = [e._id for e in extracted_data]
    params = {"change_ids": change_ids, "size": INPUT_TRACKER_DATA_LIMIT, "from": 0}
    db = create_db_connection(index)
    result = db.run_named_query("changes", ".*", params=params)
    matching_changes = dict([(r["change_id"], r["id"]) for r in result["items"]])
    update_docs: List[InputTrackerData] = []
    # Prepare input data set
    for input_tracker_data in extracted_data:
        update_docs.append(
            {
                "_id": matching_changes[input_tracker_data._id],
                "tracker_data": input_tracker_data.tracker_data,
            }
        )
    # Now insert the data
    db.update_tracker_data(source_it=update_docs)
    return jsonify([])


def main():
    app.run(host="0.0.0.0", port=9876)


if __name__ == "__main__":
    main()
