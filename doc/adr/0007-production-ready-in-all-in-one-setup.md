# 7. Production ready services in all-in-one setup

* Status: accepted
* Deciders: Frederic Lepied
* Date: 2020-05-09

## Context and Problem Statement

How do we serve the API using a production ready server?

## Decision Drivers

* use production ready server.
* good integration with reverse proxy servers like Nginx and Apache.
* support Flask in a simple and performant way

## Considered Options

* [option 1] [uWSGI](https://uwsgi-docs.readthedocs.org/)
* [option 2] [Meinheld](http://meinheld.org/)

## Decision Outcome

Chosen option: "[option 1]", because it is well known in the
industry. It is not the most performant but it has good performances
to serve WSGI app like Flask thanks to the uwsgi protocol used between
Nginx/Apache.

### Negative Consequences

* complexify the architecture.
