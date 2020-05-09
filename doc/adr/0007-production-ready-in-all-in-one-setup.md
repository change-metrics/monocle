# 7. Production ready services in all-in-one setup

* Status: accepted
* Deciders: Frederic Lepied
* Date: 2020-05-09

## Context and Problem Statement

How do we serve the API and web app from a single server on regular
http or https ports?

## Decision Drivers

* serve api and web app from the same base URL.
* support http and https.
* use production ready servers and recipes.
* support Flask in a simple and performant way

## Considered Options

* [option 1] [uWSGI](https://uwsgi-docs.readthedocs.org/) + [Nginx](http://nginx.org/)
* [option 2] [Meinheld](http://meinheld.org/) + [Gunicorn](https://gunicorn.org/)

## Decision Outcome

Chosen option: "[option 1]", because it is well known in the
industry. It is not the most performant but it has good performances
to serve WSGI app like Flask thanks to the uwsgi protocol used between
Nginx and uWSGI while at the same time being very flexible to serve
web app

### Positive Consequences

* a lot of recipes to integrate Nginx with other needed pieces like letsencrypt.

### Negative Consequences

* complexify the architecture.
