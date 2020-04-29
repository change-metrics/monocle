# Monocle

Monocle purpose is to provide metrics about Changes through a web API and a web UI. Monocle supports Github Pull Requests and Gerrit Reviews. Monocle provides a set of crawlers designed to fetch Pull Requests and Reviews data from the Github or Gerrit APIs and to store changes and changes related events into Elasticsearch. Furthermore Monocle implements ready to use queries and a React based web UI.

![Screenshot](https://user-images.githubusercontent.com/529708/78028243-fc958980-735e-11ea-8fd4-f5ecfb6af02a.png)

## Deploy the master version

Monocle is in an early phase of developement. Feedback is highly welcome.

The process below describes how to index changes from a Github repository, a full Github organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and prepare data directory

```Shell
$ git clone https://github.com/morucci/monocle.git
$ cd monocle
$ mkdir data etc dump
```

### Create the projects.yaml file

Generate a personal access token on Github (w/o any specific rights).

Then create the config file `etc/projects.yaml`:

```YAML
---
projects:
  - index: monocle
    crawler:
      loop_delay: 10
      github_orgs:
        - name: tektoncd
          repository: pipeline
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
        - name: spinnaker
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
      gerrit_repositories:
        - name: ^zuul/.*
          updated_since: "2020-03-15 00:00:00"
          base_url: https://review.opendev.org
  # A private index only whitelisted users are authorized to access
  - index: monocle-private
    users_whitelist:
      - github_login
    crawler:
      loop_delay: 10
      github_orgs:
        - name: containers
          repository: libpod
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
```

### Start docker-compose

```ShellSession
$ docker-compose up -d
```

ElasticSearch could need some capabilities to run in container
mode. Take a look at the logs to see if it started correctly:

```ShellSession
$ docker-compose logs elastic
```

For example, you could need to increase this system parameter:

```ShellSession
$ sudo sysctl -w vm.max_map_count=262144
```

or make the data directory writable for other:

```ShellSession
$ chmod o+w data
```

You might need to check the crawler logs:

```ShellSession
$ docker-compose logs crawler
```

### Accessing the web UI

You should be able to access the web UI at <http://localhost:3000/monocle>.

## Hacking

### Understanding the design choices

Follow the [Architectural Decision Records](doc/adr/index.md) to
understand the choices made by the project.

### Reloading code

This section explains how you can hack the Monocle code. The idea is to use
the docker deployment to avoid complex development methods.

After changes, simply run the following command. docker-compose will
rebuild and re-spawn the changed containers:

```ShellSession
$ docker-compose up -d --build
```

### Git hooks

#### pre-push

To be sure to push correct branches, you have to configure the
`pre-push` git hook by creating `.git/hooks/pre-push` with the
following content:

```Shell
#!/bin/bash

exec ./contrib/pre-push "$@"
```

and making it executable with `chmod +x .git/hooks/pre-push`.

#### pre-commit

Optionnaly, you can enable the `pre-commit` git hook to reformat your
code by creating `.git/hooks/pre-commit` with the following
content:

```Shell
#!/bin/bash

exec ./contrib/pre-commit "$@"
```

and making it executable with `chmod +x .git/hooks/pre-commit`.

### Configure the Github Oauth authentication to secure private indexes

1. Create an Oauth APP in your Github user settings page
2. Add "http://localhost:9876/api/0/authorize" in "User authorization callback URL"
3. Set "CLIENT_ID" and "CLIENT_SECRET" in docker-compose.yaml
4. Re/start the Docker compose
5. Navigate to "http://localhost:3000/login" to authenticate

The authentication and authorization support is new and only provides
a solution to control access to private indexes. Only login users
part of "users_whitelist" will be authorized to access the related index.
