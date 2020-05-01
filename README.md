# Monocle

Monocle purpose is to provide metrics about Changes through a web API and a web UI. Monocle supports Github Pull Requests and Gerrit Reviews. Monocle provides a set of crawlers designed to fetch Pull Requests and Reviews data from the Github or Gerrit APIs and to store changes and changes related events into Elasticsearch. Furthermore Monocle implements ready to use queries and a React based web UI.

![Screenshot](https://user-images.githubusercontent.com/529708/78028243-fc958980-735e-11ea-8fd4-f5ecfb6af02a.png)

## Installation

Monocle is in an early phase of developement. Feedback is highly welcome.

The process below describes how to index changes from a Github repository, a full Github organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and prepare data directory

```Shell
$ git clone https://github.com/morucci/monocle.git
$ cd monocle
$ mkdir data etc dump
$ ln -s docker-compose.yml.img docker-compose.yml
```

### Create the config.yaml file

The `config.yaml` file is used by the crawler and api services.

If you want to crawl Gihub repositories, generate a personal access
token on Github (w/o any specific rights).

Then create the config file `etc/config.yaml`:

```YAML
---
tenants:
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
    users:
      - <github_login1>
      - <github_login2>
    crawler:
      loop_delay: 10
      github_orgs:
        - name: containers
          repository: libpod
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
```

## Configuration of the containers

To configure the host serving the api and web UI, add
`MONOCLE_HOST=<host or ip>` into the `.env` file. If you don't
configure it, `MONOCLE_HOST` is set to `localhost`.

If you want to use a specific version, add `MONOCLE_VERSION=<version>`
into the `.env` file.

### GitHub authentication

If you want to protect the access to your indices, you can require a
GitHub login to access and the people able to use the indeices will be
the ones listed in the `users` section in `config.yaml`.

Configure the Github Oauth authentication to secure private indexes

1. Create an Oauth APP in your Github user settings page
2. Add "http://$MONOCLE_HOST:9876/api/0/authorize" in "User authorization callback URL"
3. Save the `CLIENT_ID` and `CLIENT_SECRET` into `.env` as `GITHUB_CLIENT_ID=<CLIENT_ID>` and `GITHUB_CLIENT_SECRET=<CLIENT_SECRET>`.

The authentication and authorization support is new and only provides
a solution to control access to private indexes. Only login users
part of `users` will be authorized to access the related index.

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

You should be able to access the web UI at <http://localhost:3000>.

## Contributing

Follow [our contributing guide](CONTRIBUTING.md).
