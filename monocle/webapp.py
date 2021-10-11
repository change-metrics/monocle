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
import time

from typing import Optional

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
from monocle import env


CACHE_TIMEOUT = 300  # 5 mn cache
INPUT_TASK_DATA_LIMIT = 500

cache = Cache(config={"CACHE_TYPE": "simple"})
app = Flask(__name__)
cache.init_app(app)

app.secret_key = os.urandom(16)
CORS(
    app,
    resources={r"/api/*": {"origins": os.getenv("ALLOW_ORIGIN", "*")}},
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


def returnAPIError(desc: str, code: int, details: Optional[str] = None):
    abort(
        make_response(
            jsonify({"statusCode": code, "message": desc, "details": details}), code
        )
    )


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
    redirect_uri = os.getenv("PUBLIC_URL", "")
    return github.authorize_redirect(redirect_uri + "/api/0/authorize")


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
    redirect_uri = os.getenv("PUBLIC_URL", "/")
    return redirect(redirect_uri)


@app.route("/api/0/whoami", methods=["GET"])
def whoami():
    if not os.getenv("CLIENT_ID"):
        returnAPIError("Authentication not configured", 503)
    username = session.get("username") or request.headers.get("Remote-User")
    return jsonify(username)


def get_index(req):
    if "index" not in req.args or not req.args.get("index"):
        returnAPIError("No index provided", 404)
    return req.args["index"]


@app.route("/api/0/query/<name>", methods=["GET"])
def query(name):
    index = get_index(request)
    repository_fullname = request.args.get("repository")
    try:
        ret = do_query(index, repository_fullname, request.args, name)
    except Exception:
        app.logger.exception(
            "Unable to process query %s (params: %s)"
            % (name, list(request.args.items()))
        )
        returnAPIError(
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
    params["_project_defs"] = env.project_defs.get(index)
    db = create_db_connection(index)
    try:
        result = db.run_named_query(name, repository_fullname, params)
    except InvalidIndexError:
        returnAPIError("Invalid index: %s" % request.args.get("index"), 404)
    return jsonify(result)


@app.route("/api/0/indices", methods=["GET"])
def indices():
    db = create_db_connection(None)
    _indices = db.get_indices()
    indices = []
    for indice in _indices:
        indices.append(indice)
    return jsonify(indices)


def main():
    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", 9876)))


if __name__ == "__main__":
    main()
