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

from flask import Flask
from flask import abort
from flask import jsonify
from flask import make_response
from flask import request
from flask_cors import CORS

from monocle import utils
from monocle.db.db import ELmonocleDB
from monocle.db.db import UnknownQueryException


app = Flask(__name__)
CORS(app, resources={r"/api/0/query/*": {"origins": os.getenv('ALLOW_ORIGIN', '*')}})


@app.route("/api/0/query/<name>", methods=['GET'])
def query(name):
    if not request.args.get('index'):
        abort(make_response(jsonify(errors=['No index provided']), 404))
    repository_fullname = request.args.get('repository')
    try:
        params = utils.set_params(request.args)
    except UnknownQueryException as err:
        return "Unable to process query: %s" % err, 400
    db = ELmonocleDB(
        elastic_conn=os.getenv('ELASTIC_CONN', 'localhost:9200'),
        index=request.args.get('index'),
    )
    result = db.run_named_query(name, repository_fullname, params)
    return jsonify(result)


def main():
    app.run(host='0.0.0.0', port=9876)


if __name__ == "__main__":
    main()
