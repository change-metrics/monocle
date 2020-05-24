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

import requests
import iso8601
import jwt

from typing import Dict
from typing import List
from typing import Optional

from dataclasses import dataclass, field

from datetime import timezone
from datetime import datetime
from datetime import timedelta

import logging

PREVIEW_JSON_ACCEPT = 'application/vnd.github.machine-man-preview+json'


@dataclass
class Token:
    token: str
    expiry: datetime


@dataclass
class Installation:
    id: str
    app_id: str
    app_key: str
    login: str
    account_type: str
    site_admin: str
    repository_selection: str
    access_tokens_url: str
    repositories_url: str
    token: Token
    repos: List[str] = field(init=False)


def get_app_auth_headers(app_id: str, app_key: str) -> Dict[str, str]:
    now = datetime.now(timezone.utc)
    expiry = now + timedelta(minutes=5)

    data = {'iat': now, 'exp': expiry, 'iss': app_id}
    app_token = jwt.encode(data, app_key, algorithm='RS256').decode('utf-8')

    headers = {'Accept': PREVIEW_JSON_ACCEPT, 'Authorization': 'Bearer %s' % app_token}

    return headers


def get_installation_key(install: Installation) -> str:

    now = datetime.now(timezone.utc)
    token = install.token.token
    expiry = install.token.expiry

    if (not expiry) or (not token) or (now >= expiry):
        headers = get_app_auth_headers(app_id=install.app_id, app_key=install.app_key)

        url = install.access_tokens_url
        response = requests.post(url, headers=headers, json=None)
        response.raise_for_status()

        data = response.json()

        expiry = iso8601.parse_date(data.get('expires_at'))
        if expiry:
            expiry -= timedelta(minutes=2)
        else:
            expiry = datetime.now(timezone.utc) - timedelta(minutes=-1)
        token = data.get('token', "")

        install.token.token = token
        install.token.expiry = expiry
        logging.info(
            "Refreshed application token from API for login/org: %s" % (install.login)
        )

    return token


def get_installation_headers(install: Installation,) -> Dict[str, str]:
    token = get_installation_key(install)
    return {'Accept': PREVIEW_JSON_ACCEPT, 'Authorization': 'token %s' % token}


def get_repos_of_installation(install: Installation,) -> List[str]:
    url = install.repositories_url
    headers = get_installation_headers(install)
    projects = []
    while url:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        repos = response.json()

        for repo in repos.get('repositories'):
            project = repo.get('full_name')
            projects.append(project)

        url = response.links.get('next', {}).get('url')
    install.repos = projects
    logging.info(
        "Got %s installed repositories for login/org %s"
        % (len(install.repos), install.login)
    )
    return projects


def get_installations(base_url: str, app_id: str, app_key: str) -> List[Installation]:
    url = '%s/app/installations' % base_url
    headers = get_app_auth_headers(app_id, app_key)
    installations = []
    page = 1
    while url:
        logging.info("Fetching installations for GitHub app " "(page %s)" % page)
        page += 1
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        token = Token("", datetime.now(timezone.utc) - timedelta(minutes=-1))
        _installations = response.json()
        _installations = [
            Installation(
                id=inst['id'],
                app_id=inst['app_id'],
                app_key=app_key,
                login=inst['account']['login'],
                account_type=inst['account']['type'],
                site_admin=inst['account']['site_admin'],
                repository_selection=inst['repository_selection'],
                access_tokens_url=inst['access_tokens_url'],
                repositories_url=inst['repositories_url'],
                token=token,
            )
            for inst in _installations
        ]
        installations.extend(_installations)

        url = response.links.get('next', {}).get('url')
    logging.info("Loaded %s installations for app_id:%s" % (len(installations), app_id))
    return installations


class MonocleGihtubApp:
    def __init__(
        self, app_key: str, app_id: str, base_url: str = "https://api.github.com"
    ) -> None:
        self.app_id = app_id
        self.app_key = app_key
        self.base_url = base_url
        self.installations: List[Installation] = []

    def search_installations(self) -> List[Installation]:
        self.installations = get_installations(self.base_url, self.app_id, self.app_key)
        for install in self.installations:
            get_installation_key(install)
            get_repos_of_installation(install)
        return self.installations

    def get_token(self, org: str) -> Optional[str]:
        for installation in self.installations:
            if installation.login == org:
                logging.info("Found matching app installed on %s. Use token." % org)
                return get_installation_key(installation)
        # No macthing org so fallback to use the first app token
        # that may work in case of public org
        if self.installations:
            logging.info(
                "No app installed on this org %s. Use a first app found token." % org
            )
            return get_installation_key(self.installations[0])
        logging.info("No app installed. No token to use.")
        return None


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    key_file = "app_key.rsa"
    with open(key_file, 'r') as f:
        app_key = f.read()
    app = MonocleGihtubApp(app_key=app_key, app_id="59519",)
    installations = app.search_installations()
    print(installations)
    print(app.get_token('monocle-org'))
