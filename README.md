# Monocle

The main idea behind Monocle is to detect anomalies in the way changes
are produced in your project on GitHub and Gerrit.

Each team is unique in its way of working: how to do code reviews, how
many reviewers, how to do CI, is self merge allowed...

So the philosophy behind Monocle is to let you visualize and explore
metrics and data that are relevant to the way you work by navigating
and filtering in the web user interface.

Browse changes metrics on the [demo instance](https://demo.changemetrics.io).

For example, your team may want to know:

- what is the ratio of created changes vs merged changes?
- is there a good balance between change creations and change reviews?
- what is the ratio of abandoned changes?
- what are the collaboration patterns between the team members?
- how long does it take to merge a change?
- average delay for the first comment or review?
- long standing changes?
- do we have new contributors?

Here is a graph representing what has been going on during a period of time:

![Main Stats Graph](https://user-images.githubusercontent.com/529708/80858201-0c530700-8c58-11ea-867c-9b1b4568b781.png)

Here is the graph of collaboration patterns:

![Collaboration Graph](https://user-images.githubusercontent.com/529708/80858244-79ff3300-8c58-11ea-8caa-b3e72f5d9c88.png)

Here is the graph of the complexity versus time to merge changes:

![Complexity Graph](https://user-images.githubusercontent.com/529708/80858379-45d84200-8c59-11ea-854e-9548be7968ff.png)

## Components

Monocle supports GitHub Pull Requests and Gerrit Reviews. Monocle
provides a set of crawlers designed to fetch Pull Requests and Reviews
data from the GitHub or Gerrit APIs and to store changes and changes
related events into Elasticsearch. These changes and events are exposed
via a JSON API. Furthermore Monocle implements ready to use queries
and a React based web UI.

To summarize we have the following components in Monocle:

1. an Elasticsearch data store.
2. an api service.
3. a crawler service.
4. a web UI.

## Installation

Monocle is in an early phase of development. Feedback is highly welcome.

The process below describes how to index changes from a GitHub repository, a full GitHub organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and create the needed directories

```Shell
$ git clone https://github.com/morucci/monocle.git
$ cd monocle
$ mkdir data etc dump
$ ln -s docker-compose.yml.img docker-compose.yml
```

### Create the config.yaml file

The `config.yaml` file is used by the crawler and api services.

If you want to crawl GitHub repositories, generate a personal access
token on GitHub (w/o any specific rights) at https://github.com/settings/tokens.

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
          updated_since: "2020-03-15"
          base_url: https://review.opendev.org
  # A private index only whitelisted users are authorized to access
  # See "Advanced deployment configuration" section
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

### Start docker-compose

Start Monocle:

```ShellSession
$ docker-compose up -d
```

Ensure services are running:

```ShellSession
$ docker-compose ps
monocle_api_1       uwsgi --uid guest --gid no ...   Up      0.0.0.0:9876->9876/tcp, 0.0.0.0:9877->9877/tcp
monocle_crawler_1   monocle --elastic-conn ela ...   Up
monocle_elastic_1   /usr/local/bin/docker-entr ...   Up      0.0.0.0:9200->9200/tcp, 9300/tcp
monocle_web_1       docker-entrypoint.sh /bin/ ...   Up      0.0.0.0:3000->3000/tcp
```

You might need to check the crawler logs to ensure the crawler started to fetch changes:

```ShellSession
$ docker-compose logs crawler
```

After a change in the configuration file, the api and crawler services need to be restarted:

```ShellSession
$ docker-compose restart api
$ docker-compose restart crawler
```

You should be able to access the web UI at <http://localhost:3000>.


#### Troubleshooting

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

You might want to wipe a Monocle index:

```
docker-compose run --no-deps crawler /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --index <index-name> --delete-repository ".*"
```

or delete an index:

```
docker-compose run --no-deps crawler /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --index <index-name> --delete-index
```

## Advanced deployment configuration

### Configuration of the containers

For a local deployment, default settings are fine.

The following settings are available in the `.env` file:

- `MONOCLE_URL=<host or ip>` to configure the URL serving the Web UI
  (default `http://localhost:3000`).
- `MONOCLE_API_URL=<host or ip>` to configure the URL serving the API
  (default `http://localhost:9876`).
- `MONOCLE_VERSION=<version>` to use a specific version. By default it
  uses `latest`.
- `MONOCLE_TITLE=<title>` to change the title of the web application. By
  default it is `Monocle`.
- `ES_XMS and ES_XMX` to change the ElasticSearch JVM HEAP SIZE. By default
  512m.
- `MONOCLE_API_ADDR=<ip>` to change the IP address the API service is
  listening to (default `0.0.0.0`).
- `MONOCLE_WEB_ADDR=<ip>` to change the IP address the Web service is
  listening to (default `0.0.0.0`).
- `MONOCLE_ELASTIC_ADDR=<ip>` to change the IP address the
  ElasticSearch service is listening to (default `0.0.0.0`). This is
  only exposed in the development version of the docker-compose
  (`docker-compose.yml.dev`).

### GitHub authentication

If you want to protect the access to your indices, you can require a
GitHub login to access and the people able to use the indices will be
the ones listed in the `users` section in `config.yaml`.

Configure the GitHub Oauth authentication to secure private indexes

1. [Create an Oauth APP in your GitHub user settings page](https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/)
2. Add "http://$MONOCLE_HOST:9876/api/0/authorize" in "User authorization callback URL"
3. Save the `CLIENT_ID` and `CLIENT_SECRET` into `.env` as `GITHUB_CLIENT_ID=<CLIENT_ID>` and `GITHUB_CLIENT_SECRET=<CLIENT_SECRET>`.

The authentication and authorization support is new and only provides
a solution to control access to private indexes. Only login users
part of `users` will be authorized to access the related index.

Note that the GitHub application can be also used as a Oauth APP.

### GitHub application

Monocle can interact with a GitHub application to create and use installed
application token to query the API.

Once the application is created and Monocle started with application id and
private key. If a `github_orgs` entry's token attribute is missing Monocle will
search accross the application installations for an installed application
on the related GitHub organization. If any, it will generate an installation token
for the matching installation and use it to query the GitHub API.

#### Create the application on GitHub

1. [Register new GitHub App](https://github.com/settings/apps/new)
2. In `Repository permissions` set `Metadata` as `Read-Only`,
   `Pull requests` as `Read-Only` and `Contents` as `Read-Only`
3. Click `Create the GitHub App`
4. Click `Generate a private key` and download the key
5. Save the `App ID`

#### Setup Monocle to use the application

1. Save the private key into `etc/app_key.rsa`
2. Into the `.env` file add `GITHUB_APP_ID=<APP_ID>` and `GITHUB_APP_KEY_PATH=/etc/monocle/app_key.rsa`

## Contributing

Follow [our contributing guide](CONTRIBUTING.md).
