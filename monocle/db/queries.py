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


import statistics
from elasticsearch.helpers import scan as scanner


def generate_filter(repository_fullname, kwargs):
    gte = kwargs.get('gte')
    lte = kwargs.get('lte')
    etype = kwargs.get('etype')
    author = kwargs.get('author')
    approval = kwargs.get('approval')
    state = kwargs.get('state')

    if isinstance(etype, str):
        etype = list(etype)

    created_at_range = {
        "created_at": {
            "format": "epoch_millis"
        }
    }
    if gte:
        created_at_range['created_at']['gte'] = gte
    if lte:
        created_at_range['created_at']['lte'] = lte
    qfilter = [
        {"regexp": {
            "repository_fullname": {
                "value": repository_fullname}}},
        {"range": created_at_range}
    ]
    if etype:
        qfilter.append({"terms": {"type": etype}})
    if author:
        qfilter.append({"term": {"author": author}})
    if state:
        qfilter.append({"term": {"state": state}})
    if approval:
        qfilter.append({'term': {"approval": approval}})
    return qfilter


def generate_must_not(kwargs):
    must_not = []
    if kwargs['exclude_authors']:
        must_not.append(
            {
                "terms": {
                    "author": kwargs['exclude_authors']
                }
            }
        )
    return must_not


def set_kwargs_defaults(kwargs):
    " Apply default values to kwargs"
    return {
        'gte': kwargs.get('gte'),
        'lte': kwargs.get('lte'),
        'etype': kwargs.get('etype'),
        'author': kwargs.get('author'),
        'interval': kwargs.get('interval', '3h'),
        'size': kwargs.get('size', 10),
        'exclude_authors': kwargs.get('exclude_authors', []),
        'approval': kwargs.get('approval'),
    }


def run_query(es, index, body):
    params = {'index': index, 'doc_type': index}
    params['body'] = body
    try:
        res = es.search(**params)
    except Exception:
        return []
    return res


def _scan_events(es, index, repository_fullname, **kwargs):
    params = {'index': index, 'doc_type': index}
    body = {
        # "_source": "repository_fullname_and_number",
        "_source": kwargs['field'],
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    params['query'] = body
    data = scanner(es, **params)
    return [d for d in data]


# TODO (Change type is not an event an must be discarded)
def count_events(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    body = {
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    params = {'index': index, 'doc_type': index}
    params['body'] = body
    try:
        res = es.count(**params)
    except Exception:
        return {}
    return res['count']


def count_authors(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    body = {
        "aggs": {
            "agg1": {
                "cardinality": {
                    "field": "author",
                    "precision_threshold": 3000,
                }
            }
        },
        "size": 0,
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    data = run_query(es, index, body)
    return data['aggregations']['agg1']['value']


def events_histo(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    body = {
        "aggs": {
            "agg1": {
                "date_histogram": {
                    "field": "created_at",
                    "interval": kwargs['interval'],
                }
            },
            "avg_count": {
                "avg_bucket": {
                    "buckets_path": "agg1>_count"
                }
            }
        },
        "size": 0,
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    data = run_query(es, index, body)
    return (
        data['aggregations']['agg1']['buckets'],
        data['aggregations']['avg_count']['value'])


def _events_top(
        es, index, repository_fullname, field, **kwargs):
    body = {
        "aggs": {
            "agg1": {
                "terms": {
                    "field": field,
                    "size": kwargs['size'],
                    "order": {
                        "_count": "desc"
                    }
                }
            }
        },
        "size": 0,
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    data = run_query(es, index, body)
    count_series = [
        b['doc_count'] for b in
        data['aggregations']['agg1']['buckets']]
    count_avg = (statistics.mean(count_series)
                 if count_series else 0)
    count_median = (statistics.median(sorted(count_series))
                    if count_series else 0)
    return {
        'buckets': data['aggregations']['agg1']['buckets'],
        'count_avg': count_avg, 'count_median': count_median}


def events_top_authors(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    return _events_top(
        es, index, repository_fullname, "author", **kwargs)


def changes_top_approval(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "ChangeReviewedEvent"
    return _events_top(
        es, index, repository_fullname, "approval", **kwargs)


def changes_top_commented(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "ChangeCommentedEvent"
    return _events_top(
        es, index, repository_fullname, "repository_fullname_and_number",
        **kwargs)


def changes_top_reviewed(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "ChangeReviewedEvent"
    return _events_top(
        es, index, repository_fullname, "repository_fullname_and_number",
        **kwargs)


def authors_top_reviewed(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "ChangeReviewedEvent"
    return _events_top(
        es, index, repository_fullname, "on_author", **kwargs)


def authors_top_commented(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "ChangeCommentedEvent"
    return _events_top(
        es, index, repository_fullname, "on_author", **kwargs)


def peers_exchange_strength(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = ("ChangeReviewedEvent", "ChangeCommentedEvent")
    authors = [bucket['key'] for bucket in _events_top(
        es, index, repository_fullname, "author", **kwargs)['buckets']]
    peers_strength = {}
    for author in authors:
        kwargs['author'] = author
        for bucket in _events_top(
                es, index, repository_fullname, "on_author",
                **kwargs)['buckets']:
            if bucket['key'] == author:
                continue
            peers_id = tuple(sorted((author, bucket['key'])))
            peers_strength.setdefault(peers_id, 0)
            peers_strength[peers_id] += bucket['doc_count']
    peers_strength = sorted(
        [(peers_id, strength) for peers_id, strength in
         peers_strength.items()],
        key=lambda x: x[1], reverse=True)
    return(peers_strength)


def change_merged_count_by_duration(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "Change"
    kwargs['state'] = "MERGED"
    body = {
        "aggs": {
            "agg1": {
                "range": {
                    "field": "duration",
                    "ranges": [
                        {
                            "to": 24*3600
                        },
                        {
                            "from": 24*3600+1,
                            "to": 7*24*3600
                        },
                        {
                            "from": 7*24*3600+1,
                            "to": 31*24*3600
                        },
                        {
                            "from": 31*24*3600+1
                        },
                    ],
                    "keyed": True
                }
            }
        },
        "size": 0,
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    data = run_query(es, index, body)
    return data['aggregations']['agg1']['buckets']


def pr_merged_avg_duration(es, index, repository_fullname, **kwargs):
    kwargs = set_kwargs_defaults(kwargs)
    kwargs['etype'] = "Change"
    kwargs['state'] = "MERGED"
    body = {
        "aggs": {
            "agg1": {
                "avg": {
                    "field": "duration"
                }
            }
        },
        "size": 0,
        "docvalue_fields": [
            {
                "field": "created_at",
                "format": "date_time"
            },
        ],
        "query": {
            "bool": {
                "filter": generate_filter(repository_fullname, kwargs),
                "must_not": generate_must_not(kwargs)
            }
        }
    }
    data = run_query(es, index, body)
    return data['aggregations']['agg1']
