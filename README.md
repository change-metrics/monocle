# Monocle

The purpose of Monocle is to provide insights on the way changes are
produced in your team on GitHub and Gerrit.

The philosophy is to let you visualize and explore metrics that are
relevant to the way you work.

For example, your team may want to know:

- what is the ratio of created changes vs merged changes?
- is there a good balance between change creation and change reviews?
- what is the ratio of abandoned changes?
- what are the collaboration patterns between the team members?
- how long does it take to merge a change?
- average delay for the first comment or review?
- long standing changes?
- do we have new contributors?

The main idea behind Monocle is to detect anomalies in your development process.

Here is a graph representing what has been going on during a period of time:

![Main Stats Graph](https://user-images.githubusercontent.com/529708/80858201-0c530700-8c58-11ea-867c-9b1b4568b781.png)

Here is the collaboration pattern:

![Collaboration Graph](https://user-images.githubusercontent.com/529708/80858244-79ff3300-8c58-11ea-8caa-b3e72f5d9c88.png)

Here is the complexity versus time to merge changes:

![Complexity Graph](https://user-images.githubusercontent.com/529708/80858379-45d84200-8c59-11ea-854e-9548be7968ff.png)

## Components

Monocle supports Github Pull Requests and Gerrit Reviews. Monocle
provides a set of crawlers designed to fetch Pull Requests and Reviews
data from the Github or Gerrit APIs and to store changes and changes
related events into Elasticsearch. These changes and events are expose
via a JSON API. Furthermore Monocle implements ready to use queries
and a React based web UI.

To summarize we have the following components in Monocle:

1. an Elasticsearch data store.
2. an api service.
3. a crawler service.
4. a web UI.

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
