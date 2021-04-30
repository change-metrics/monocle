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
import sys
import yaml
from typing import Dict, List
from monocle import config

config_path = os.getenv("CONFIG", None)
if not config_path:
    print("CONFIG env is missing.", file=sys.stderr)
    indexes_acl: Dict[str, List[config.Username]] = {}
    project_defs: Dict[str, List[config.ProjectDefinition]] = {}
    indexes_task_crawlers = {}
else:
    if not os.path.isfile(config_path):
        print("Unable to access %s." % config_path, file=sys.stderr)
        sys.exit(1)
    else:
        rawconfig = yaml.safe_load(open(config_path))
        indexes_acl = config.build_index_acl(rawconfig)
        project_defs = config.build_project_definitions(rawconfig)
        indexes_task_crawlers = config.build_index_task_crawlers(rawconfig)
