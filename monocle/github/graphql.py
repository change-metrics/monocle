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
import requests

from time import sleep
from datetime import datetime

from tenacity import retry, wait_fixed, after_log, stop_after_attempt


class Timeout(Exception):
    pass


class GithubGraphQLQuery(object):

    log = logging.getLogger(__name__)

    def __init__(self, token):
        self.url = 'https://api.github.com/graphql'
        self.headers = {
            'Authorization': 'token %s' % token,
            'User-Agent': 'change-metrics/monocle',
        }
        self.session = requests.session()
        # Will get every 25 requests
        self.get_rate_limit_rate = 25
        self.query_count = 0
        # Set an initial value
        self.quota_remain = 5000
        self.get_rate_limit()
        self.retry_after = False

    def get_rate_limit(self):
        try:
            ratelimit = self.getRateLimit()
        except requests.exceptions.ConnectionError:
            sleep(5)
            ratelimit = self.getRateLimit()
        self.quota_remain = ratelimit['remaining']
        self.resetat = datetime.strptime(ratelimit['resetAt'], '%Y-%m-%dT%H:%M:%SZ')
        self.log.info(
            "Got rate limit data: remain %s resetat %s"
            % (self.quota_remain, self.resetat)
        )

    # https://developer.github.com/v3/guides/best-practices-for-integrators/#dealing-with-abuse-rate-limits
    def wait_for_call(self):
        if self.retry_after:
            self.log.info("Waiting for %s secs (Retry-After)" % self.retry_after)
            sleep(self.retry_after)
            self.retry_after = False
        elif self.quota_remain <= 150:
            until_reset = self.resetat - datetime.utcnow()
            self.log.info(
                "Quota remain: %s/calls delay until "
                "reset: %s/secs waiting ..." % (self.quota_remain, until_reset.seconds)
            )
            sleep(until_reset.seconds + 60)
            self.get_rate_limit()
        else:
            self.log.debug("Sleeping 1 sec to be a good citizen")
            sleep(1)

    def getRateLimit(self):
        qdata = '''{
          rateLimit {
            limit
            cost
            remaining
            resetAt
          }
        }'''
        data = self.query(qdata, skip_get_rate_limit=True)
        return data['data']['rateLimit']

    @retry(
        after=after_log(log, logging.INFO),
        wait=wait_fixed(1),
        stop=stop_after_attempt(1),
        reraise=True,
    )
    def query(self, qdata, skip_get_rate_limit=False):
        if not skip_get_rate_limit:
            if self.query_count % self.get_rate_limit_rate == 0:
                self.get_rate_limit()
            self.wait_for_call()
        data = {'query': qdata}
        r = self.session.post(
            url=self.url, json=data, headers=self.headers, timeout=30.3
        )
        self.query_count += 1
        if 'retry-after' in r.headers:
            self.log.info('Got Retry-After: %s' % r.headers['retry-after'])
            self.retry_after = int(r.headers['retry-after'])
        if not r.status_code != "200":
            self.log.error('No ok response code: %s' % r)
            raise Exception("No ok response code: %s" % r.text)
        ret = r.json()
        if 'errors' in ret:
            self.log.error("Errors in response: %s" % ret)
            if (
                len(ret['errors']) >= 1
                and 'message' in ret['errors'][0]
                and 'timeout' in ret['errors'][0]['message']
            ):
                raise Timeout(ret['errors'][0]['message'])
            raise Exception("Errors in response: %s" % ret['errors'])
        return ret
