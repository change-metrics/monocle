# A python benchmark equivalent to the JsonDecode.hs
import time
import os
import statistics
import json
import iso8601


def get_duration(hit):
    return iso8601.parse_date(hit["created_at"]) - iso8601.parse_date(
        hit["on_created_at"]
    )


def run(dat):
    # Decode json value
    b = json.loads(dat)
    print("Loading took %.3fms" % (1000 * (time.monotonic() - before)))

    # Compute duration
    return statistics.mean(
        [get_duration(hit["_source"]).total_seconds() for hit in b["hits"]["hits"]]
    )


before = time.monotonic()
dat = open(os.environ.get("FP")).read()
result = run(dat)
print("Total        %.3fms" % (1000 * (time.monotonic() - before)))
print("Result: ", result)
