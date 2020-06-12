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
import socket
import time

from datetime import datetime

from elasticsearch.helpers import bulk
from elasticsearch import client
from elasticsearch.exceptions import NotFoundError

from monocle.db import queries
from monocle import utils

CHANGE_PREFIX = 'monocle.changes.'


class UnknownQueryException(Exception):
    pass


InvalidIndexError = NotFoundError


class ELmonocleDB:

    log = logging.getLogger("monocle.ELmonocleDB")

    def __init__(
        self,
        elastic_conn='localhost:9200',
        index=None,
        timeout=10,
        prefix=CHANGE_PREFIX,
        create=True,
    ):
        host, port = elastic_conn.split(':')
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        ip = socket.gethostbyname(host)
        self.log.info('ES IP is %s' % ip)
        self.log.info('ES prefix is %s' % prefix)

        while True:
            try:
                s.connect((ip, int(port)))
                s.shutdown(2)
                s.close()
                break
            except Exception as excpt:
                self.log.info(
                    'Unable to connect to %s: %s. Sleeping for %ds.'
                    % (elastic_conn, excpt, timeout)
                )
                time.sleep(timeout)

        self.log.info('Connecting to ES server at %s' % elastic_conn)
        self.es = client.Elasticsearch(elastic_conn)
        self.log.info(self.es.info())

        self.prefix = prefix

        if not index:
            self.log.info('No index provided')
            return

        self.index = '{}{}'.format(self.prefix, index)
        self.log.info('Using ES index %s' % self.index)
        self.mapping = {
            self.index: {
                "properties": {
                    "id": {"type": "keyword"},
                    "type": {"type": "keyword"},
                    "number": {"type": "keyword"},
                    "change_id": {"type": "keyword"},
                    "title": {
                        "type": "text",
                        "fields": {
                            "keyword": {"type": "keyword", "ignore_above": 8191}
                        },
                    },
                    "text": {
                        "type": "text",
                        "fields": {
                            "keyword": {"type": "keyword", "ignore_above": 8191}
                        },
                    },
                    "url": {"type": "keyword"},
                    "commit_count": {"type": "integer"},
                    "additions": {"type": "integer"},
                    "deletions": {"type": "integer"},
                    "changed_files_count": {"type": "integer"},
                    "changed_files": {
                        "properties": {
                            "additions": {"type": "integer"},
                            "deletions": {"type": "integer"},
                            "path": {"type": "keyword"},
                        }
                    },
                    "commits": {
                        "properties": {
                            "sha": {"type": "keyword"},
                            "author": {"type": "keyword"},
                            "committer": {"type": "keyword"},
                            "authored_at": {
                                "type": "date",
                                "format": "date_time_no_millis",
                            },
                            "committed_at": {
                                "type": "date",
                                "format": "date_time_no_millis",
                            },
                            "additions": {"type": "integer"},
                            "deletions": {"type": "integer"},
                            "title": {"type": "text"},
                        }
                    },
                    "repository_prefix": {"type": "keyword"},
                    "repository_fullname": {"type": "keyword"},
                    "repository_shortname": {"type": "keyword"},
                    "author": {"type": "keyword"},
                    "on_author": {"type": "keyword"},
                    "committer": {"type": "keyword"},
                    "merged_by": {"type": "keyword"},
                    "branch": {"type": "keyword"},
                    "target_branch": {"type": "keyword"},
                    "created_at": {"type": "date", "format": "date_time_no_millis"},
                    "on_created_at": {"type": "date", "format": "date_time_no_millis"},
                    "merged_at": {"type": "date", "format": "date_time_no_millis"},
                    "updated_at": {"type": "date", "format": "date_time_no_millis"},
                    "closed_at": {"type": "date", "format": "date_time_no_millis"},
                    "state": {"type": "keyword"},
                    "duration": {"type": "integer"},
                    "mergeable": {"type": "keyword"},
                    "label": {"type": "keyword"},
                    "assignee": {"type": "keyword"},
                    "approval": {"type": "keyword"},
                    "draft": {"type": "boolean"},
                }
            }
        }
        settings = {'mappings': self.mapping}
        self.ic = client.IndicesClient(self.es)
        if create:
            self.ic.create(index=self.index, ignore=400, body=settings)

    def update(self, source_it):
        def gen(it):
            for source in it:
                d = {}
                d['_index'] = self.index
                d['_type'] = self.index
                d['_op_type'] = 'update'
                d['_id'] = source['id']
                d['doc'] = source
                d['doc_as_upsert'] = True
                yield d

        bulk(self.es, gen(source_it))
        self.es.indices.refresh(index=self.index)

    def delete_index(self):
        self.log.info('Deleting index: %s' % self.index)
        self.ic.delete(index=self.index)

    def delete_repository(self, repository_fullname):
        params = {'index': self.index, 'doc_type': self.index}
        body = {
            "query": {
                "bool": {
                    "filter": {
                        "regexp": {
                            "repository_fullname": {"value": repository_fullname}
                        }
                    }
                }
            }
        }
        params['body'] = body
        self.es.delete_by_query(**params)
        self.es.indices.refresh(index=self.index)

    def get_last_updated(self, repository_fullname):
        params = {'index': self.index, 'doc_type': self.index}
        body = {
            "sort": [{"updated_at": {"order": "desc"}}],
            "query": {
                "bool": {
                    "filter": [
                        {"term": {"type": "Change"}},
                        {
                            "regexp": {
                                "repository_fullname": {"value": repository_fullname}
                            }
                        },
                    ]
                }
            },
        }
        params['body'] = body
        try:
            res = self.es.search(**params)
        except Exception:
            return []
        ret = [r['_source'] for r in res['hits']['hits']]
        if not ret:
            return []
        return ret[0]

    def run_named_query(self, name, *args, **kwargs):
        # Here we set gte and lte if not provided by user
        # especially to be able to set the histogram extended_bounds
        if name not in queries.public_queries:
            raise UnknownQueryException("Unknown query: %s" % name)
        if not args[1].get('gte'):
            first_created_event = queries._first_created_event(
                self.es, self.index, *args, **kwargs
            )
            if first_created_event:
                args[1]['gte'] = int(
                    utils.dbdate_to_datetime(first_created_event).timestamp() * 1000
                )
            else:
                # There is probably nothing the db that match the query
                args[1]['gte'] = None
        if not args[1].get('lte'):
            args[1]['lte'] = int(datetime.now().timestamp() * 1000)
        return getattr(queries, name)(self.es, self.index, *args, **kwargs)

    def get_indices(self):
        return [
            ind.replace(self.prefix, '')
            for ind in self.es.indices.get(self.prefix + '*')
        ]
