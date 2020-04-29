# Monocle.
# Copyright (C) 2019-2020 Monocle authors
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
import sys
import yaml

from typing import Dict, List

from flask import Flask
from flask import abort
from flask import jsonify
from flask import make_response
from flask import request
from flask import redirect
from flask import session
from flask_cors import CORS

# https://docs.authlib.org/en/latest/client/frameworks.html#frameworks-clients
# https://docs.authlib.org/en/latest/client/flask.html#flask-client
from authlib.integrations.flask_client import OAuth

from monocle import utils
from monocle.db.db import CHANGE_PREFIX
from monocle.db.db import ELmonocleDB
from monocle.db.db import UnknownQueryException, InvalidIndexError
from monocle import projects

app = Flask(__name__)

app.secret_key = os.urandom(16)
CORS(
    app,
    resources={r"/api/0/*": {"origins": os.getenv('ALLOW_ORIGIN', '*')}},
    supports_credentials=True,
)
oauth = OAuth(app)

oauth.register(
    name='github',
    client_id=os.getenv('CLIENT_ID'),
    client_secret=os.getenv('CLIENT_SECRET'),
    access_token_url='https://github.com/login/oauth/access_token',
    access_token_params=None,
    authorize_url='https://github.com/login/oauth/authorize',
    authorize_params=None,
    api_base_url='https://api.github.com/',
    client_kwargs={'scope': 'user:email'},
)

indexes_acl: Dict[str, List[projects.Username]] = {}


@app.route("/api/0/login", methods=['GET'])
def login():
    github = oauth.create_client('github')
    redirect_uri = os.getenv('REDIRECT_URL')
    return github.authorize_redirect(redirect_uri)


@app.route("/api/0/authorize", methods=['GET'])
def authorize():
    github = oauth.create_client('github')
    # token = github.authorize_access_token()
    github.authorize_access_token()
    resp = github.get('user')
    profile = resp.json()
    # do something with the token and profile
    session['username'] = profile.get('login')
    # return jsonify(profile)
    return redirect('http://localhost:3000/monocle')


@app.route("/api/0/whoami", methods=['GET'])
def whoami():
    username = session.get('username')
    return jsonify(username)


@app.route("/api/0/query/<name>", methods=['GET'])
def query(name):
    if not request.args.get('index'):
        abort(make_response(jsonify(errors=['No index provided']), 404))
    index = request.args.get('index')
    if not projects.is_public_index(indexes_acl, index):
        user = session.get('username')
        if user:
            if user not in projects.get_authorized_users(indexes_acl, index):
                return 'Unauthorized to access index %s' % index, 503
        else:
            return 'Unauthorized to access index %s' % index, 503

    repository_fullname = request.args.get('repository')
    try:
        params = utils.set_params(request.args)
    except UnknownQueryException as err:
        return "Unable to process query: %s" % err, 400
    db = ELmonocleDB(
        elastic_conn=os.getenv('ELASTIC_CONN', 'localhost:9200'),
        index=index,
        prefix=CHANGE_PREFIX,
        create=False,
    )
    try:
        result = db.run_named_query(name, repository_fullname, params)
    except InvalidIndexError:
        return 'Invalid index: %s' % request.args.get('index'), 404
    return jsonify(result)


@app.route("/api/0/indices", methods=['GET'])
def indices():
    db = ELmonocleDB(
        elastic_conn=os.getenv('ELASTIC_CONN', 'localhost:9200'),
        create=False,
        prefix=CHANGE_PREFIX,
    )
    _indices = db.get_indices()
    indices = []
    for indice in _indices:
        if projects.is_public_index(indexes_acl, indice):
            indices.append(indice)
        else:
            user = session.get('username')
            if user:
                if user in projects.get_authorized_users(indexes_acl, indice):
                    indices.append(indice)
    return jsonify(indices)


def main():
    projects_config = os.getenv('PROJECTS_YAML', None)
    if not projects_config:
        print('PROJECTS_YAML env is missing. Quit')
        sys.exit(1)
    if not os.path.isfile(projects_config):
        print('Unable to access %s. Quit' % projects_config)
        sys.exit(1)
    globals()['indexes_acl'] = projects.build_index_acl(
        yaml.safe_load(open(projects_config))
    )
    app.run(host='0.0.0.0', port=9876)


if __name__ == "__main__":
    main()
