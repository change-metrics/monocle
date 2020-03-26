# MIT License
# Copyright (c) 2019 Fabien Boucher

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


from elasticsearch.helpers import bulk
from elasticsearch import client

from monocle.db import queries
from monocle import utils

from datetime import datetime


class ELmonocleDB():

    def __init__(self, index='monocle'):
        self.es = client.Elasticsearch('localhost:9200')
        self.index = index
        self.mapping = {
            self.index: {
                "properties": {
                    "id": {"type": "keyword"},
                    "type": {"type": "keyword"},
                    "number": {"type": "keyword"},
                    "repository_fullname_and_number": {"type": "keyword"},
                    "title": {"type": "keyword"},
                    "url": {"type": "keyword"},
                    "commits": {"type": "keyword"},
                    "additions": {"type": "integer"},
                    "deletions": {"type": "integer"},
                    "changed_files": {"type": "integer"},
                    "repository_prefix": {"type": "keyword"},
                    "repository_fullname": {"type": "keyword"},
                    "repository_shortname": {"type": "keyword"},
                    "author": {"type": "keyword"},
                    "on_author": {"type": "keyword"},
                    "committer": {"type": "keyword"},
                    "merged_by": {"type": "keyword"},
                    "branch": {"type": "keyword"},
                    "target_branch": {"type": "keyword"},
                    "created_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "on_created_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "merged_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "updated_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "closed_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "authored_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "committed_at": {
                        "type": "date",
                        "format": "date_time_no_millis"
                    },
                    "state": {"type": "keyword"},
                    "duration": {"type": "integer"},
                    "mergeable": {"type": "keyword"},
                    "label": {"type": "keyword"},
                    "assignee": {"type": "keyword"},
                    "approval": {"type": "keyword"},
                }
            }
        }
        settings = {
            'mappings': self.mapping
        }
        self.ic = client.IndicesClient(self.es)
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

    def delete_repository(self, repository_fullname):
        params = {'index': self.index, 'doc_type': self.index}
        body = {
            "query": {
                "bool": {
                    "filter": {
                        "regexp": {
                            "repository_fullname": {
                                "value": repository_fullname
                            }
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
            "sort": [{
                "updated_at": {
                    "order": "desc"
                }
            }],
            "query": {
                "bool": {
                    "filter": [
                        {"term": {"type": "Change"}},
                        {"regexp": {
                            "repository_fullname": {
                                "value": repository_fullname
                            }
                        }}
                    ]
                }
            }
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
        if not args[1].get('gte'):
            first_created_event = queries._first_created_event(
                    self.es, self.index, *args, **kwargs)
            if first_created_event:
                args[1]['gte'] = int(utils.dbdate_to_datetime(
                    first_created_event).timestamp() * 1000)
            else:
                # There is probably nothing the db that match the query
                args[1]['gte'] = None
        if not args[1].get('lte'):
            args[1]['lte'] = int(datetime.now().timestamp() * 1000)
        return getattr(queries, name)(
            self.es, self.index, *args, **kwargs)
