# Monocle

The main idea behind Monocle is to detect anomalies in the way changes
are produced in your project on GitHub, GitLab and Gerrit.

Monocle is in an early phase of development. Feedback is highly welcome.

Checkout the [changemetrics.io website](https://changemetrics.io) and
join us in the Matrix room at [#monocle:matrix.org](https://matrix.to/#/#monocle:matrix.org).

We are currently working on the [version 1.0 roadmap](https://changemetrics.io/posts/2021-07-06-v1-roadmap.html).

## Components

![architecture](./doc/architecture.png)

Monocle is composed of the following services:

1. an Elasticsearch data store.
2. an api service to perform user query and index crawler output.
3. a crawler service to retrieve change from provider.
4. a web proxy and web application to browse metrics.

The APIs are defined using [protobuf][monocle-protobuf] and served over HTTP through [Monocle OpenAPI][monocle-openapi].

Some legacy component are still required until they are migrated to the new OpenAPI (see the related [topic](https://github.com/change-metrics/monocle/labels/legacy)):

5. an api service to perform filter based query.
6. a crawler service to index github and gerrit changes.

## Installation

The process below describes how to index changes from a GitHub repository, a full GitHub organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and create the needed directories

```Shell
$ git clone https://github.com/change-metrics/monocle.git
$ cd monocle
$ git submodule update --init --recursive
$ echo CRAWLERS_API_KEY=$(uuidgen) > .secrets
$ ln -s docker-compose.yml.img docker-compose.yml
```

By default docker-compose will fetch the latest published container images.
Indeed, we produce Docker container images for the master version of Monocle.
If running master does not fit your needs, you could still use the last release
by setting the MONOCLE_VERSION to 0.9.0 in the .env file. Please refer
to [Configuration of the containers](#configuration-of-the-containers).

### Create the config.yaml file

The `config.yaml` file is used by the crawler and api services.

If you want to crawl GitHub public repositories, generate a personal access
token on GitHub (w/o any specific rights) at https://github.com/settings/tokens.
In case of GitHub private repositories, see the [GitHub private repositories](#github-private-repositories) section.

Then create the config file `etc/config.yaml`. Here is an example your could start with. Make sure to write `GITHUB_TOKEN=<github_token>` in the `.secrets` file:

```YAML
---
workspaces:
  - name: monocle
    crawlers:
      - name: github-tektoncd
        provider:
          github_organization: tektoncd
          github_repositories:
            - operator
            - pipeline
        update_since: '2020-05-01'
```

To crawl the full tektoncd GitHub organization then remove the `github_repositories` entry from the file.
A more complete example is available in the section [Full configuration file example](#full-configuration-file-example).

### Start docker-compose

Start Monocle:

```ShellSession
$ docker-compose up -d
```

Ensure services are running:

```ShellSession
$ docker-compose ps
```

You might need to check the crawler logs to ensure the crawler started to fetch changes:

```ShellSession
$ docker-compose logs -f crawler
$ docker-compose logs -f crawler-legacy
```

You should be able to access the web UI at <http://localhost:8080>.

After a change in the configuration file, the api and crawler services need to be restarted:

```ShellSession
$ docker-compose restart api-legacy
$ docker-compose restart crawler-legacy
```

## Advanced configuration

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

### Monocle configuration file

The Monocle configuration file defines workspaces. The file is used by the API and crawlers processes. The format of the file is YAML. You might want to use Dhall to manage it or to better
understand the schema ([dhall-monocle](https://github.com/change-metrics/dhall-monocle.)).

#### Workpaces

A workspace uses a dedicated ElasticSearch index. A workspace defines:

- crawlers - [details](#crawlers)
- projects - [details](#projects-definition)
- identities - [details](#groups-definition)

#### Crawlers

Monocle provides two kinds of crawlers:

- Change: A crawler to fetch Changes proposed to a repository. Monocle supports Gerrit (Reviews), GitHub (Pull-Requests), GitLab (Merge-Requests).
- TaskData: A crawler to fetch task data related to a repository. Monocle supports GitHub (issues),
  and BugZilla (Bugs).

The `.secrets` file is used to store credentials and API keys used by crawlers to authenticate on providers.

The `crawlers` value is a list of crawler. Each crawler is composed of:

- `name`: an abitrary name used to identify the crawler.
- `updated_since`: the crawler will fetch changes that has been created/updated since that date.
- `provider`: provider settings

```YAML
workspaces:
  - name: demo
    crawlers:
      - name: spinnaker
        updated_since: "2020-01-01"
        provider: {}
```

##### Change

###### GitHub

A GitHub provider settings

```YAML
  provider:
    github_organization: spinnaker
    # Optional settings
    github_repositories:
      - pipeline
    github_url: https://github.com
    github_token: GITHUB_TOKEN
```

`github_organization` is the only mandatory key. If `github_repositories` is not specified then
the crawler will crawl the whole organization repositories. If specified then it will crawl only
the listed repositories.

`github_url` might be specified in case of an alternate url. Default is "github.com".

`github_token` might be specified to use an alternate environment variable name to look for the
token value. Default is "GITHUB_TOKEN"

To crawl privates repositories, either you must use a [GitHub Application](#github-application)
or you must generate a Personal Access Token with the "repo" scope.

Note that this crawler is managed by the `crawler-legacy` container.

###### Gerrit

A Gerrit provider settings

```YAML
  provider:
    gerrit_url: https://review.opendev.org
    gerrit_repositories:
      - openstack/nova
      - openstack/neutron
    # Optional settings
    gerrit_login: monocle
    gerrit_password: GERRIT_PASSWORD
    gerrit_url_insecure: true
    gerrit_prefix: opendev/
```

`gerrit_url` is mandatory and must be the url of the Gerrit provider.
`gerrit_repositories` is mandatory and is the list of repositories from which the crawler will fetch Reviews from.

`gerrit_login` might be specified to authenticate on the provider API.
`gerrit_password`might be specified to use an alternate environment variable name to look for the
password. Default is "GERRIT_PASSWORD"

`gerrit_url_insecure` could be set to true to prevent HTTP certificate check.

`gerrit_prefix` might be set to configure the crawler to prepend the repository name with a prefix.

Note that this crawler is managed by the `crawler-legacy` container.

###### GitLab

A GitLab provider settings

```YAML
  provider:
    gitlab_organization: redhat/centos-stream/ci-cd/zuul
    # Optional settings
    gitlab_repositories:
      - jobs-config
    gitlab_url: https://gitlab.com/api/graphql
    gitlab_token: GITLAB_TOKEN
```

`gitlab_organization` is the only mandatory key. If `gitlab_repositories` is not specified then
the crawler will crawl the whole organization repositories. If specified then it will crawl only
the listed repositories.

`gitlab_url` might be specified in case of an alternate url. Default is "https://gitlab.com/api/graphql".

`gitlab_token` might be specified to use an alternate environment variable name to look for the
token value. Default is "GITLAB_TOKEN"

Note that this crawler is managed by the `crawler` container.

##### TaskData

Monocle provides additional crawlers to attach tasks/issues/RFEs to changes based on a
match on `change_url`. Then, Changes can be enhanced with information about related
tasks such as a `priority` or a `score`.

However these crawlers are not fully integrated with the configuration file and the crawler
container process. Until they are, usage might be a bit rough.

###### GitHub

```ShellSession
export MONOCLE_API_KEY=$(sed -n '/CRAWLERS_API_KEY=/ s/CRAWLERS_API_KEY=//p' .secrets)
docker-compose run --rm --no-deps crawler lentille-github --monocle-url http://api-legacy:9896 --index monocle.changes.1.my-index --crawler-name my-crawler --since "2020-01-01T00:00:00Z" --repo change-metrics/monocle
```

###### Bugzilla

```ShellSession
export MONOCLE_API_KEY=$(sed -n '/CRAWLERS_API_KEY=/ s/CRAWLERS_API_KEY=//p' .secrets)
docker-compose run --rm --no-deps crawler lentille-bugzilla --monocle-url http://api-legacy:9896 --index monocle.changes.1.my-index --crawler-name my-crawler --since "2020-01-01T00:00:00Z" --bugzilla-product Awesome-Product
```

### Projects definition

Projects could be defined within a workspace configuration. A project is identified by a name and allows to set the following filter attributes:

- repository_regex
- branch_regex
- file_regex

Here is an example of configuration.

```YAML
workspaces:
  - name: example
    crawlers:
      - name: openstack
        provider:
          gerrit_url: https://review.opendev.org
          gerrit_repositories:
            - ^openstack/.*
        updated_since: "2000-01-01"
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
retrieved the list defined projects for a given workspace. See the
[Monocle OpenAPI][monocle-openapi].

The monocle query endpoint handles the query parameter: `project`.

### Identity Management

Monocle is able to index changes from multiple code review systems. A contributor
might get different identities across code review systems. Thus Monocle provides
a configuration section to define aliases for contributors.

Let say a Monocle workspace is configured to fetch changes from github.com and
review.opendev.org (Gerrit) and we would like that John's metrics are merged under
the `John Doe` identity.

```YAML
workspaces:
  - name: example
    idents:
      - ident: John Doe
        aliases:
          - github.com/john-doe
          - review.opendev.org/John Doe/12345
    crawlers:
      - name: github-containers
        provider:
          github_organization: containers
          github_token: <github_token>
        update_since: '2000-01-01'

      - name: gerrit-opendev
        provider:
          gerrit_url: https://review.opendev.org
          gerrit_repositories:
            - ^openstack/.*
        update_since: '2000-01-01'
```

A contributor id on github.com or a GitHub enterprise instance is formated as `<domain>/<login>`.

A contributor id on a Gerrit instance is formated as `<domain>/<Full Name>/<gerrit-user-id>`.

### Groups definition

A group in Monocle permits to group authors of Changes and filter them from the web interface.

Group memberships are defined through the [idents](#identity-management) section of the configuration.

Here is an example:

```YAML
workspaces:
  - name: example
    idents:
      - ident: John Doe
        aliases:
          - github.com/john-doe
          - review.opendev.org/John Doe/12345
        groups:
          - devs
      - ident: Jane Doe
        aliases:
          - github.com/jane-doe
        groups:
          - devs
          - ptl
```

#### Apply idents configuration

Database objects must be updated to reflect the configuration. Once `config.yaml` is updated, run the following commands:

```bash
docker-compose restart crawler-legacy
docker-compose run --rm --no-deps crawler-legacy /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --workspace <workspace-name> --config /etc/monocle/config.yaml --update-idents
docker-compose restart api-legacy
```

### Full configuration file example

Here are the expected environment variables:

- `CRAWLERS_API_KEY`: an arbitrary api key used by the crawler to index data.
- `GITHUB_TOKEN`: an API key for GitHub crawler.
- `GITLAB_TOKEN`: an API key for GitLab crawler.
- Optional `GERRIT_PASSWORD`: an account password for Gerrit crawler.

To use a different variable name, add a new attribute to the crawler definition using the default name in lowercase, `github_token: CUSTOM_ENV_NAME`.

```YAML
---
workspaces:
  - name: monocle
    crawlers:
      - name: tektoncd
        provider:
          github_organization: tektoncd
          github_repositories:
            - pipeline
        updated_since: "2020-03-15"
      - name: spinnaker
        updated_since: "2020-03-15"
        provider:
          github_organization: spinnaker
          github_repositories:
            - pipeline
  - name: zuul
    crawlers:
      - name: gerrit-opendev
        provider:
          gerrit_url: https://review.opendev.org
          gerrit_repositories:
            - ^zuul/.*
        update_since: '2020-03-15'
  - name: openstack
    idents:
      - ident: "Fabien Boucher"
        aliases:
          - "review.opendev.org/Fabien Boucher/6889"
          - "review.rdoproject.org/Fabien Boucher/112"
    crawlers:
      - name: bz-crawler
        updated_since: "2021-01-01"
        provider: TaskDataProvider
      - name: gerrit-opendev
        provider:
          gerrit_url: https://review.opendev.org
          gerrit_repositories:
            - ^openstack/.*
        update_since: '2020-03-15'
      - name: gerrit-rdo
        provider:
          gerrit_url: https://review.rdoproject.org/r/
          gerrit_repositories:
            - ^openstack/.*
        update_since: '2020-03-15'
```

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
2. Into the `.secrets` file add `GITHUB_APP_ID=<APP_ID>` and `GITHUB_APP_KEY_PATH=/etc/monocle/app_key.rsa`

## Database migration

### From version 0.8.X to next stable

Identities are consolidated in the database, to enable multiple code review identities (across code review systems) to be grouped.

1. Run the migration process for each workspace

```
docker-compose stop
docker-compose start elastic
# For each workspace
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --workspace <workspace-name> --run-migrate from-0.8-to-last-stable
docker-compose up -d
```

### From version 0.7.0

A new field `self_merged` has been added. Previously indexed changes can be updated by running the `self-merge` migration process.

```
docker-compose run --rm --no-deps crawler /usr/local/bin/monocle --elastic-conn elastic:9200 dbmanage --workspace <index-name> --run-migrate self-merge
```

## Using external authentication system

Monocle is supporting the "REMOTE_USER" header, which is mostly used
by sign-on solutions. When Web server takes care of authentitaction,
it set a "REMOTE_USER" environment variable, which can be used by Monocle.
To check that, you are able to run simple curl command:

```Shell
curl --header "REMOTE_USER: Daniel" -XGET http://localhost:9876/api/0/whoami
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

You might want to wipe a Monocle project:

```
docker-compose run --rm --no-deps crawler-legacy /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --workspace <workspace-name> --delete-repository ".*"
```

or delete a workspace:

```
docker-compose run --rm --no-deps crawler-legacy /usr/local/bin/monocle \
--elastic-conn elastic:9200 dbmanage --workspace <workspace-name> --delete-workspace
```

## Contributing

Follow [our contributing guide](CONTRIBUTING.md).

[monocle-protobuf]: ./protos/monocle
[monocle-openapi]: ./doc/openapi.yaml
