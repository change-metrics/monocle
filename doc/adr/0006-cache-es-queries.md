# 6. Cache ES queries

* Status: accepted
* Deciders: Frederic Lepied
* Date: 2020-05-08

## Context and Problem Statement

To handle the load of multiple connected users, the API service is
doing always the same Elasticsearch requests for the same indices. How
do we minmize this number of requests?

## Decision Drivers

We need a simple solution to start that is integrating well with our
existing choices and that has the capacity to evolve into a more
distributed solution if needed in the future.

## Considered Options

* [option 1] do caching in front of the API service with a reverse proxy.
* [option 2] do caching inside the API service.

## Decision Outcome

Chosen option: "[option 1]", because we have an optional
authentication scheme that would have beed difficult to manage with
"[option 2]".

We picked [Flask Caching](https://pythonhosted.org/Flask-Caching/)
using Simple caching in memory and the memoize method to cache the
queries for some time. It has the capacity to be used in a distrubted
setup later if needed.

### Positive Consequences

* using [siege](https://github.com/JoeDog/siege) we were able to
  simulate 100 users without problems on a single laptop.
