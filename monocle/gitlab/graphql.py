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

import logging
from time import sleep

import requests
from monocle import utils
from tenacity import (
    after_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_fixed,
)


class RequestTimeout(Exception):
    pass


class RequestException(Exception):
    pass


class GitlabGraphQLQuery(object):

    log = logging.getLogger(__name__)

    def __init__(self, token_getter):
        self.session = requests.session()
        self.get_rate_limit_rate = 25
        self.query_count = 0
        self.quota_remain = 5000
        self.token_getter = token_getter

    def get_token(self) -> str:
        return self.token_getter.get_token()[0]

    def get_headers(self) -> dict:
        headers = {
            "Authorization": f"Bearer {self.get_token()}",
            "Content-Type": "application/json",
            "User-Agent": "change-metrics/monocle",
        }
        self.log.debug(f"request headers: {headers}")
        return headers

    def get_rate_limit(self):
        ratelimit = self.getRateLimit()
        if ratelimit:
            self.quota_remain = ratelimit["remaining"]
            self.resetat = utils.is8601_to_dt(ratelimit["resetAt"])
            self.log.info(
                f"Got rate limit data: remain {self.quota_remain} resetat {self.resetat}"
            )

    def wait_for_call(self):
        if self.quota_remain <= 150:
            until_reset = self.resetat - utils.utcnow()
            self.log.info(
                f"Quota remain: {self.quota_remain}/calls delay until "
                f"reset: {until_reset.seconds}/secs waiting ..."
            )
            sleep(until_reset.seconds + 60)
            self.get_rate_limit()
        else:
            self.log.debug("Sleeping 1 sec to be a good citizen")
            sleep(1)

    def getRateLimit(self):
        qdata = """{
          rateLimit {
            limit
            cost
            remaining
            resetAt
          }
        }"""
        data = self.query(qdata, skip_get_rate_limit=True)
        if data:
            try:
                return data["data"]["rateLimit"]
            except KeyError:
                self.log.error(f"No rate limit data: {data}")
                raise RequestException(f"No rate limit data: {data}")

    @retry(
        after=after_log(log, logging.INFO),
        wait=wait_fixed(10),
        stop=stop_after_attempt(30),
        retry=retry_if_exception_type(RequestException),
        reraise=True,
    )
    def query(self, qdata, skip_get_rate_limit=False):
        if not skip_get_rate_limit:
            if self.query_count % self.get_rate_limit_rate == 0:
                self.get_rate_limit()
            self.wait_for_call()
        data = {"query": qdata}
        try:
            r = self.session.post(
                url=self.url,
                json=data,
                headers=self.get_headers(),
                timeout=30.3,
            )
        except (
            requests.exceptions.ConnectionError,
            requests.exceptions.ChunkedEncodingError,
        ):
            raise RequestException("Error connecting to the API")
        self.query_count += 1
        if "retry-after" in r.headers:
            self.log.info(f'Got Retry-After: {r.headers["retry-after"]}, sleeping...')
            sleep(int(r.headers["retry-after"]))
        if not r.status_code != "200":
            self.log.error(f"No ok response code: {r}")
            raise RequestException(f"No ok response code: {r.text}")
        ret = r.json()
        if "Bad credentials" == ret.get("message", ""):
            self.log.info("Query forbidden due to bad credentials")
            ret = {}
        if "errors" in ret:
            self.log.error(f"Errors in response: {ret}")
            if (
                len(ret["errors"]) >= 1
                and "message" in ret["errors"][0]
                and "timeout" in ret["errors"][0]["message"]
            ):
                raise RequestTimeout(ret["errors"][0]["message"])
            if len(ret["errors"]) >= 1:
                if all(
                    [
                        error
                        for error in ret["errors"]
                        if "The additions count for this commit is unavailable"
                        in error["message"]
                    ]
                ):
                    return ret
            is_forbiden = any([error["type"] == "FORBIDDEN"] for error in ret["errors"])
            if is_forbiden:
                self.log.info("Query forbidden due to unsuffcient token ACLs")
                ret = {}
            else:
                raise RequestException(f'Errors in response: {ret["errors"]}')
        return ret
