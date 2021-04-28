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

from datetime import datetime

from typing import Dict, List, Optional, Union, Tuple
from dataclasses import asdict

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
from monocle.task_data import (
    createInputTaskData,
    createELTaskData,
    TaskDataForEL,
    OrphanTaskDataForEL,
    TaskCrawler,
)
from monocle import config


CACHE_TIMEOUT = 300  # 5 mn cache
INPUT_TASK_DATA_LIMIT = 500

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
        globals()["indexes_task_crawlers"] = config.build_index_task_crawlers(rawconfig)


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
        returnAPIError("Authentication not configured", 503)
    username = session.get("username") or request.headers.get("Remote-User")
    return jsonify(username)


def get_index(req):
    if "index" not in req.args or not req.args.get("index"):
        returnAPIError("No index provided", 404)
    return req.args["index"]


@app.route("/api/0/projects", methods=["GET"])
def get_project_definition():
    index = get_index(request)
    if index not in project_defs:
        return returnAPIError("No index with this name", 404)
    return jsonify([asdict(p) for p in project_defs[index]])


@app.route("/api/0/query/<name>", methods=["GET"])
def query(name):
    index = get_index(request)
    if not config.is_public_index(indexes_acl, index):
        user = session.get("username") or request.headers.get("Remote-User")
        if user:
            if user not in config.get_authorized_users(indexes_acl, index):
                returnAPIError("Unauthorized to access index %s" % index, 403)
        else:
            returnAPIError("Unauthorized to access index %s" % index, 403)
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
    params["_project_defs"] = project_defs.get(index)
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
        if config.is_public_index(indexes_acl, indice):
            indices.append(indice)
        else:
            user = session.get("username")
            if user:
                if user in config.get_authorized_users(indexes_acl, indice):
                    indices.append(indice)
    return jsonify(indices)


def task_data_endpoint_check_input_env(
    req, check_auth: bool, check_content_type: bool
) -> Tuple[str, TaskCrawler]:
    if "index" not in req.args or not req.args.get("index"):
        returnAPIError("No index provided", 404)
    index = req.args["index"]
    if index not in globals()["indexes_task_crawlers"]:
        return returnAPIError("No index with this name", 404)
    if "name" not in req.args or not req.args.get("name"):
        return returnAPIError("No crawler name provided", 404)
    name = req.args["name"]
    match_crawler_config = [
        c for c in globals()["indexes_task_crawlers"][index] if c.name == name
    ]
    if not match_crawler_config:
        return returnAPIError("No crawler with this name", 404)
    crawler_config = match_crawler_config[0]
    apikey = None
    if check_auth:
        if "apikey" not in req.args and not req.args.get("apikey"):
            return returnAPIError("No crawler apikey provided", 404)
        apikey = req.args["apikey"]
        if apikey != crawler_config.api_key:
            return returnAPIError("Not authorized", 403)
    if check_content_type:
        if not req.is_json:
            return returnAPIError("Missing content-type application/json", 400)
    return index, crawler_config


@app.route("/api/0/task_data/commit", methods=["POST"])
def task_data_commit():
    index, crawler_config = task_data_endpoint_check_input_env(
        request, check_auth=True, check_content_type=True
    )
    input_date_str = request.get_json()
    try:
        input_date = datetime.strptime(input_date_str, "%Y-%m-%dT%H:%M:%SZ")
    except ValueError:
        returnAPIError("Unable to read input date", 400)
    db = create_db_connection(index)
    if db.set_task_crawler_metadata(crawler_config.name, input_date):
        returnAPIError("Unable to commit", 500)
    return jsonify("Commited")


@app.route("/api/0/task_data", methods=["POST", "GET"])
def task_data():
    if request.method == "POST":
        index, crawler_config = task_data_endpoint_check_input_env(
            request, check_auth=True, check_content_type=True
        )
        json_data: List = request.get_json()
        if not isinstance(json_data, list):
            returnAPIError("Input data is not a List", 400)
        if len(json_data) > INPUT_TASK_DATA_LIMIT:
            returnAPIError(
                "Input data List over limit (%s items)" % (INPUT_TASK_DATA_LIMIT),
                400,
            )
        try:
            extracted_data = createInputTaskData(json_data, crawler_config.name)
        except Exception as exc:
            returnAPIError(
                "Unable to extract input data due to wrong input format: %s" % exc, 400
            )
        # Find changes in EL ids that match urls
        change_urls = [e.change_url for e in extracted_data]
        db = create_db_connection(index)
        mc = db.get_changes_by_url(change_urls, INPUT_TASK_DATA_LIMIT)
        me = db.get_change_events_by_url(change_urls)
        mc = dict(
            [
                (
                    r["url"],
                    {
                        "id": r["id"],
                        "td": createELTaskData(r.get("tasks_data", [])),
                    },
                )
                for r in mc
            ]
        )
        # Prepare input data set
        update_docs: List[Union[TaskDataForEL, OrphanTaskDataForEL]] = []
        for input_task_data in extracted_data:
            if input_task_data.change_url in mc:
                # First check if a td match the input one
                prev_td = [
                    td
                    for td in mc[input_task_data.change_url]["td"]
                    if td.url == input_task_data.url
                ]
                if len(prev_td) > 1:
                    raise RuntimeError("Multiple td match in previous td")
                # Remove the previous outdated one if any
                if prev_td:
                    mc[input_task_data.change_url]["td"].remove(prev_td[0])
                # Add the new one to the list
                mc[input_task_data.change_url]["td"].append(input_task_data)
            else:
                update_docs.append(
                    OrphanTaskDataForEL(
                        _id=input_task_data.url,
                        task_data=input_task_data,
                    )
                )
        total_orphans_to_update = len(update_docs)
        for _mc in mc.values():
            update_docs.append(
                TaskDataForEL(
                    _id=_mc["id"],
                    tasks_data=_mc["td"],
                )
            )
        total_changes_to_update = len(update_docs) - total_orphans_to_update
        for _me in me:
            update_docs.append(
                TaskDataForEL(_id=_me["id"], tasks_data=mc[_me["url"]]["td"])
            )
        total_change_events_to_update = (
            len(update_docs) - total_orphans_to_update - total_changes_to_update
        )
        # Now insert the data
        err = db.update_task_data(source_it=update_docs)
        # https://github.com/elastic/elasticsearch-py/blob/f4447bf996bdee47a0eb4c736bd39dea20a4486e/elasticsearch/helpers/actions.py#L177
        if err:
            returnAPIError("Unable to update tasks data", 500, str(err))
        db.set_task_crawler_metadata(
            crawler_config.name,
            push_infos={
                "last_post_at": datetime.utcnow().replace(microsecond=0),
                "total_docs_posted": len(extracted_data),
                "total_changes_updated": total_changes_to_update,
                "total_change_events_updated": total_change_events_to_update,
                "total_orphans_updated": total_orphans_to_update,
            },
        )
        return jsonify([])
    if request.method == "GET":
        index, crawler_config = task_data_endpoint_check_input_env(
            request, check_auth=False, check_content_type=False
        )
        db = create_db_connection(index)
        metadata = db.get_task_crawler_metadata(crawler_config.name)
        if "details" in request.args and request.args.get("details") == "true":
            return jsonify(metadata)
        if not metadata.get("last_commit_at"):
            commit_date = crawler_config.updated_since.strftime("%Y-%m-%dT%H:%M:%S")
        else:
            commit_date = metadata["last_commit_at"]
        return jsonify(commit_date + "Z")


def main():
    app.run(host="0.0.0.0", port=9876)


if __name__ == "__main__":
    main()
