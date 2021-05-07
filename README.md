# Monocle

The main idea behind Monocle is to detect anomalies in the way changes
are produced in your project on GitHub and Gerrit.

Each team is unique in its way of working: how to do code reviews, how
many reviewers, how to do CI, is self merge allowed...

So the philosophy behind Monocle is to let you visualize and explore
metrics and data that are relevant to the way you work by navigating
and filtering in the web user interface.

Browse changes metrics on the [demo instance](https://demo.changemetrics.io).
Follow us on [@change_metrics on Twitter](https://twitter.com/change_metrics).

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

OpenAPI definitions are availables: [Monocle OpenAPI][monocle-openapi].

## Installation

Monocle is in an early phase of development. Feedback is highly welcome.

The process below describes how to index changes from a GitHub repository, a full GitHub organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and create the needed directories

```Shell
$ git clone https://github.com/change-metrics/monocle.git
$ cd monocle
$ ln -s docker-compose.yml.img docker-compose.yml
```

By default docker-compose will fetch the latest published container images.
Indeed, we produce Docker container images for the master version of Monocle.
If running master does not git your needs, you could still use the last release
by setting the MONOCLE_VERSION to 0.8.1 in the .env file. Please refer
to [Configuration of the containers](#configuration-of-the-containers).

### Create the config.yaml file

The `config.yaml` file is used by the crawler and api services.

If you want to crawl GitHub public repositories, generate a personal access
token on GitHub (w/o any specific rights) at https://github.com/settings/tokens.
In case of GitHub private repositories, see the [GitHub private repositories](#github-private-repositories) section.

Then create the config file `etc/config.yaml`. Here is an example your could start with. Make sure to replace `<github_token>` by your personal access token:

```YAML
---
tenants:
  - index: monocle
    crawler:
      loop_delay: 10
      github_orgs:
        - name: tektoncd
          repository: pipeline
          updated_since: "2020-05-01"
          token: <github_token>
          base_url: https://github.com
```

To crawl the full tektoncd GitHub organization then remove the _repository_ entry from the file.
A more complete example is available in the section [Full configuration file example](#full-configuration-file-example).

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
monocle_web_1       docker-entrypoint.sh /bin/ ...   Up      0.0.0.0:8080->8080/tcp
```

You might need to check the crawler logs to ensure the crawler started to fetch changes:

```ShellSession
$ docker-compose logs -f crawler
```

You should be able to access the web UI at <http://localhost:8080>.

After a change in the configuration file, the api and crawler services need to be restarted:

```ShellSession
$ docker-compose restart api
$ docker-compose restart crawler
```

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
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --index <index-name> --delete-repository ".*"
```

or delete an index:

```
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --index <index-name> --delete-index
```

## Advanced deployment configuration

### Configuration of the containers

For a local deployment, default settings are fine.

The following settings are available in the `.env` file:

- `MONOCLE_PUBLIC_URL=<url>` to configure the
  public URL to access the UI and API. This is required for the github
  oauth redirection.
- `MONOCLE_VERSION=<version>` to use a specific version. By default it
  uses `latest`.
- `MONOCLE_TITLE=<title>` to change the title of the web application. By
  default it is `Monocle`.
- `ES_XMS and ES_XMX` to change the ElasticSearch JVM HEAP SIZE. By default
  512m.
- `MONOCLE_API_ADDR=<ip>` to change the IP address the API service is
  listening to (default `0.0.0.0`).
- `MONOCLE_API_PORT=<port>` to change the port of the API (default `9876)`
- `MONOCLE_WEB_ADDR=<ip>` to change the IP address the Web service is
  listening to (default `0.0.0.0`).
- `MONOCLE_WEB_PORT=<port>` to change the port of the WEB (default `8080`)
- `MONOCLE_ELASTIC_ADDR=<ip>` to change the IP address the
- `MONOCLE_ELASTIC_PORT=<port>` to change the port of the ELK (default `9200`)
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

### GitHub private repositories

To let Monocle crawl and index privates repositories, either you must use a
[GitHub Application](#github-application) or you must generate a Personal Access Token
with the "repo" scope.

### Projects definition

Projects could be defined within an index configuration. A project is identified by a name and allows to set the following filter attributes:

- repository_regex
- branch_regex
- file_regex

Here is an example of configuration.

```YAML
tenants:
  - index: example
    crawler:
      loop_delay: 300
      gerrit_repositories:
        - name: ^openstack/.*
          updated_since: "2000-01-01"
          base_url: https://review.opendev.org
    projects:
      - name: compute
        repository_regex: ".*nova.*"
      - name: compute-tests
        file_regex: "test[s]/.*"
        repository_regex: ".*nova.*"
      - name: deployment
        repository_regex: ".*tripleo.*|.*puppet.*|.*ansible.*"
        branch_regex: "master"
```

The monocle API endpoint `api/1/get_projects` can be queried to
retrieved the list defined projects for a given index. See the
[Monocle OpenAPI][monocle-openapi].

The monocle query endpoint handles the query parameter: `project`.

### Identity Management

Monocle is able to index changes from multiple code review systems. A contributor
might get different identities across code review systems. Thus Monocle provides
a configuration section to define aliases for contributors.

Let say a Monocle index is configured to fetch changes from github.com and review.opendev.org (Gerrit) and we would like that John's metrics are merged under the `John Doe` identity.

```YAML
tenants:
  - index: example
    idents:
      - ident: John Doe
        aliases:
          - github.com/john-doe
          - review.opendev.org/John Doe/12345
    crawler:
      loop_delay: 300
      github_orgs:
        - name: containers
          updated_since: "2000-01-01"
          base_url: https://github.com
      gerrit_repositories:
        - name: ^openstack/.*
          updated_since: "2000-01-01"
          base_url: https://review.opendev.org
```

A contributor id on github.com or a GitHub enterprise instance is formated as `<domain>/<login>`.

A contributor id on a Gerrit instance is formated as `<domain>/<Full Name>/<gerrit-user-id>`.

#### Apply idents configuration

Database objects must be updated to reflect the configuration. Once `config.yaml` is updated, run the following commands:

```bash
docker-compose stop crawler
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --index <index-name> --config /etc/monocle/config.yaml --update-idents
docker-compose restart api
docker-compose start crawler
```

### Connect a tasks tracker crawler

The Monocle API provides endpoints for a crawler to send task/issue/RFE
related data into the Monocle database. Monocle is able to attach tasks
to changes based on a match of the `change_url` field.

#### Endpoints available for a crawler

**POST /api/0/task_data?index=<name>&name=<crawler_name>&apikey=<key>**

This endpoint is protected by an API Key and it accepts a list of task data.

```Shell
[TASK_DATA]

TASK_DATA: {
  updated_at: str(%Y-%m-%dT%H:%M:%SZ)
  change_url: str
  ttype: List[str]
  tid: str
  url: str
  title: str
  severity: str
  priority: str
  score: str
}
```

Check the OpenAPI definitions for tasks data endpoints: [Monocle OpenAPI][monocle-openapi].

#### Task data crawler configuration

```YAML
tenants:
  - index: default
    task_crawlers:
      - name: crawler_name
        updated_since: "2020-01-01"
        api_key: 1a2b3c4d5e
```

The `updated_since` date is the initial date the crawler needs to crawl from. Without any prior commit, a `GET` call on `/api/0/task_data` returns
the initial date.

#### Lentille - a task crawler library

[Lentille](https://github.com/change-metrics/lentille) provides Haskell modules (Worker.hs and Client.hs) to ease the development of a task crawler for Monocle.

### Full configuration file example

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
  - index: zuul
    crawler:
      loop_delay: 600
      gerrit_repositories:
        - name: ^zuul/.*
          updated_since: "2020-03-15"
          base_url: https://review.opendev.org
  - index: openstack
    idents:
      - ident: "Fabien Boucher"
        aliases:
          - "review.opendev.org/Fabien Boucher/6889"
          - "review.rdoproject.org/Fabien Boucher/112"
    task_crawlers:
      - name: bz-crawler
        updated_since: "2021-01-01"
        api_key: 1a2b3c4d5e
    crawler:
      loop_delay: 600
      gerrit_repositories:
        - name: "^openstack/.*"
          updated_since: "2021-01-01"
          base_url: https://review.opendev.org/
        - name: "^openstack/.*"
          updated_since: "2021-01-01"
          base_url: https://review.rdoproject.org/r/
          prefix: "rdo/"
  # A private index. Only whitelisted users are authorized to access
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

## Database migration

### From version 0.8.X to next stable

Identities are consolidated in the database, to enable multiple code review identities (across code review systems) to be grouped.

1. Run the migration process for each index

```
docker-compose stop
docker-compose start elastic
# For each indexes
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --index <index-name> --run-migrate from-0.8-to-last-stable
docker-compose up -d
```

### From version 0.7.0

A new field `self_merged` has been added. Previously indexed changes can be updated by running the `self-merge` migration process.

```
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --index <index-name> --run-migrate self-merge
```

## Using external authentication system

Monocle is supporting the "REMOTE_USER" header, which is mostly used
by sign-on solutions. When Web server takes care of authentitaction,
it set a "REMOTE_USER" environment variable, which can be used by Monocle.
To check that, you are able to run simple curl command:

```Shell
curl --header "REMOTE_USER: Daniel" -XGET http://localhost:9876/api/0/whoami
```

## Contributing

Follow [our contributing guide](CONTRIBUTING.md).

[monocle-openapi]: ./doc/openapi.yaml
