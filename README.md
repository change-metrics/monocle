# Monocle

Monocle is designed for development teams to provide:

- analytics on project changes
- boards to display changes by criteria

It helps teams and individual to better organize daily duties
and to detect anomalies in the way changes are produced and reviewed.

Monocle supports GitHub, GitLab and Gerrit.

How to get started with Monocle:

- Explore the website and blog: [changemetrics.io](https://changemetrics.io)
- Try on the demo instance: [demo.changemetrics.io](https://demo.changemetrics.io)
- Chat with us in the project Matrix room: [#monocle:matrix.org](https://matrix.to/#/#monocle:matrix.org)
- Ask and Discuss about Monocle on the [Discussions page](https://github.com/change-metrics/monocle/discussions)
- Run your own instance: [Read the installation guide](#installation)
- Hack on it: [Read the contributing guide](CONTRIBUTING.md)
- Checkout [who use Monocle](#users-of-monocle)

## Screenshots of Monocle from the demo instance

The activity view:

<img src="https://raw.githubusercontent.com/change-metrics/monocle/assets/images/monocle-1.3.0/monocle-activity.png" width="70%" height="70%" />

The developer board:

<img src="https://raw.githubusercontent.com/change-metrics/monocle/assets/images/monocle-1.3.0/monocle-board.png" width="70%" height="70%" />

The peers strength view:

<img src="https://raw.githubusercontent.com/change-metrics/monocle/assets/images/monocle-1.3.0/monocle-peers-strength.png" width="70%" height="70%" />

## Installation

The process below describes how to index changes from GitHub repositories
and then how to start the web UI to browse metrics.

The deployment is based on Docker via a docker-compose definition.

Alternatively, the following deployment method are supported:

- [deploy on Kubernetes](k8s/README.md)
- [deploy from source code](CONTRIBUTING.md#deploy-from-source-code)

### Checkout the code

```Shell
git clone https://github.com/change-metrics/monocle.git
cd monocle
# Init a .secrets file with a default API key for the crawler process
echo CRAWLERS_API_KEY=$(uuidgen) > .secrets
```

### Create the config.yaml file

The `config.yaml` file is used by the crawler and api services.

To crawl GitHub public repositories, you must generate a personal access
token on GitHub (w/o any specific rights) at https://github.com/settings/tokens.

Then create the config file `etc/config.yaml`. Here is an example your could start with.
Make sure to write `GITHUB_TOKEN=<github_token>` in the `.secrets` file:

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
        update_since: '2021-01-01'
```

To crawl the full tektoncd GitHub organization then remove the `github_repositories` entry from the file.
Check the section [Workspaces](#workspaces) for a complete description of
the configuration.

### Start docker-compose

The docker-compose file is set to use the container image of the latest stable
release of Monocle (1.8.0). It is adviced to use the latest stable Monocle version.
However, as our CI publishes the latest (devel) container image then it is possible
to run the very last version. To do so set `COMPOSE_MONOCLE_VERSION` to `latest` in
the `.env` file.

Please refer to the [Environment variables section](#environment-variables).

Start Monocle:

```ShellSession
docker-compose up -d
```

Ensure services are running and healthy:

```ShellSession
docker-compose ps
```

Inspect services logs:

```ShellSession
docker-compose logs -f
```

You should be able to access the web UI at <http://localhost:8080>.

See [Troubleshooting](#troubleshooting) section if needed.


## Configuration

The Monocle configuration is splitted between a configuration file and various environment
variables.

[Environment variables](#environment-variables) configure system settings and secrets. Any changes require
a restart of the Monocle API.

[The configuration file](#configuration-file) contains non sensitive data and could be exposed in a Git repository.
A CI/CD flow could be enabled on top of the Git repository to allow Monocle' users to
propose configuration changes. The Monocle API automatically reload the configuration when it changes.

### Environment variables

For a local deployment, default settings are fine.

The following settings are available in the `.env` file (See `.env.sample` for a sample file):

- `COMPOSE_ES_XMS and COMPOSE_ES_XMX` to change the ElasticSearch JVM HEAP SIZE. By default 512m.
- `COMPOSE_MONOCLE_VERSION` to use a specific version. By default it uses latest stable release version.
- `COMPOSE_MONOCLE_API_ADDR` to set binding address where the Monocle API is exposed by the container.
- `COMPOSE_MONOCLE_API_PORT` to set binding port where the Monocle API is exposed by the container.
- `COMPOSE_MONOCLE_PUBLIC_URL` to configure the public URL to access the UI and API.
  The URL is required for user redirection during the authentication. By default it is `http://localhost:8080`
- `COMPOSE_MONOCLE_WEBAPP_TITLE` to change the title of the web application. By default it is `Monocle`.

The following settings are available in the `.secrets` file:

- `MONOCLE_JWK_GEN_KEY` to set the local JWT issuer key. The key size must be 64 characters minimum.
  By default the key is automatically generated.
- `MONOCLE_OIDC_<PROVIDER_NAME>_CLIENT_SECRET` to set the secret used by Monocle to request an ID Token (Unset by default).
- `MONOCLE_ADMIN_TOKEN` to set the *token* to access admin endpoints.
  By default not set and endpoints deactivated.
### Configuration file

The Monocle configuration file is used by the API and crawlers processes. The format of the file is YAML.

The file configures the following:

- [Monocle workspaces](#workspaces)
- The [About](#about) WEB Application endpoint
- The [Authentication](#authentication) system

You might want to use Dhall to manage it or to better understand the schema ([dhall-monocle](https://github.com/change-metrics/dhall-monocle)).

#### Workspaces


A workspace uses a dedicated ElasticSearch index. A workspace defines:

- crawlers - [details](#crawlers)
- projects - [details](#projects-definition)
- identities - [details](#identity-management)
- groups - [details](#groups-definition)
- search aliases - [details](#search-aliases-definition)

##### Crawlers

Monocle provides two kinds of crawlers:

- [Change](#change): A crawler to fetch Changes proposed to a repository. Monocle supports Gerrit (Reviews), GitHub (Pull-Requests), GitLab (Merge-Requests).
- [TaskData](#taskdata): A crawler to fetch task data related to a repository. Monocle supports GitHub (issues),
  and BugZilla (Bugs).

The `.secrets` file is used to store credentials and API keys used by crawlers to authenticate on providers.

The `crawlers` value is a list of crawler. Each crawler is composed of:

- `name`: an abitrary name used to identify the crawler.
- `update_since`: the crawler will fetch changes that has been created/updated since that date.
- `provider`: provider settings

```YAML
workspaces:
  - name: demo
    crawlers:
      - name: spinnaker
        update_since: "2020-01-01"
        provider: {}
```

###### Change

A GitHub provider settings

```YAML
  provider:
    github_organization: spinnaker
    # Optional settings
    github_repositories:
      - pipeline
    github_url: https://github.com/api/graphql
    github_token: GITHUB_TOKEN
```

`github_organization` is the only mandatory key. If `github_repositories` is not specified then
the crawler will crawl the whole organization repositories. If specified then it will crawl only
the listed repositories. To crawl repositories from a personnal GitHub account, you need to set
`github_organization` to you account name and list repositories under the `github_repositories` key.

`github_url` might be specified in case of an alternate url. Default is "https://github.com/api/graphql".

`github_token` might be specified to use an alternate environment variable name to look for the
token value. Default is "GITHUB_TOKEN"

Regarding the Github tokens (classic):

- To crawl public repositories, no specific scope are needed (no checkbox selected).
- To crawl privates repositories, you must set the "repo" scope.

Regarding the Github fine grained tokens (new):

- To crawl public repositories, keep the "Public Repositories (read-only)" checkbox selected. Nothing else is needed.
- To crawl privates repositories, select "All repositories" or "Only select repositories", then in "Repository permissions"
  select "Pull Requests", "Contents" as "Read-only".

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
    gerrit_prefix: opendev/
```

`gerrit_url` is mandatory and must be the url of the Gerrit provider.
`gerrit_repositories` is mandatory and is the list of repositories from which the crawler will fetch Reviews from.

`gerrit_login` might be specified to authenticate on the provider API.
`gerrit_password`might be specified to use an alternate environment variable name to look for the
password. Default is "GERRIT_PASSWORD"

`gerrit_prefix` might be set to configure the crawler to prepend the repository name with a prefix.


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

To crawl privates repositories, you must generate a Personal Access Token with the "read_api" scope.

###### TaskData

Monocle provides additional crawlers to attach tasks/issues/RFEs to changes based on a
match on `change_url`. Then, Changes are enhanced with information about related
tasks such as a `priority` or a `score`.

For GitHub:

The GitHub TaskData crawler run automatically whenever repositories are specified into a
[GitHub Changes crawler](#github) definition.

When using the "Fine grained" token for private repositories, then make sure to select the "Issues" permission.

For Bugzilla:

A BugZilla provider settings

```YAML
  provider:
    bugzilla_url: https://redhat.bugzilla.com
    bugzilla_products:
      - Awesome product
    # Optional settings
    bugzilla_token: BUGZILLA_TOKEN
```

`bugzilla_product` must be specified. The crawler will crawl listed products for bugs that
contain an external bug references `ext_bz_bug_id`. The crawler assumes that the external
reference is used to link to a change (Pull-Request/Review).

`bugzilla_token` might be specified to use an alternate environment variable name to look for the
token value. Default is "BUGZILLA_TOKEN"

Note that this crawler is managed by the `crawler` container.

##### Projects definition

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
        update_since: "2021-01-01"
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

##### Identity Management

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
          - AuthProviderUID:jdoe
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

###### Apply idents configuration

Database objects must be updated to reflect the configuration. Once `config.yaml` is updated, run the following commands:

```bash
docker-compose run --rm --no-deps api monocle janitor update-idents --elastic elastic:9200 --config /etc/monocle/config.yaml
```

##### Groups definition

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


##### Search aliases definition

The Monocle configuration file provides a way to define aliases to be used in search queries.
A use case could be to group `bot` authors to provide an easy way to exclude them from search results.

Here is an example:

```YAML
workspaces:
  - name: example
    search_aliases:
      - name: bots
        alias: '(author:"github-actions" or author:"bedevere-bot")'
```

Then the query could be: "from:now-3weeks and not bot".

#### About

This section configures information to be displayed by the WEB Application on the `About` modal.

Here is an example:

```YAML
about:
  links:
    - category: Community
      name: Monocle configuration
      url: https://github.com/change-metrics/demo-node-config
```

#### Crawlers

This section configures global crawlers setting.

Here is an example:

```YAML
crawlers:
  # Wait one hour between iteration
  loop_delay_sec: 3600
```

#### Authentication

Monocle supports user authentication via an OIDC (OpenID Connect) provider.

Once authenticated, Monocle is able to display personalized content like resolving the
*self* query value such as in the query: *author: self*.

Monocle supports one provider at a time.

Note that by default and if authentication system is not configured or misconfigured,
Monocle provides an *unauthentcated login* mechanism which allows a user to enter its
*author name*.

##### OIDC Provider

Here is an example of configuration:

```YAML
auth:
  auth_provider:
    oidc_client_id: my_client_id
    oidc_issuer_url: https://accounts.google.com
    oidc_provider_name: my provider
    oidc_user_claim: email
  enforce_auth: false
```

- oidc_client_id: is the client id that is provided by your OIDC provider.
- oidc_issuer_url: is the URL used to discover your OIDC's provider
  configuration (via the *.well-known/openid-configuration* endpoint).
- oidc_provider_name: is the name displayed on the authentication modal in the WEB Application.
- oidc_user_claim: (optional) is the JWT claim used as/(or to discover matching) Monocle User ID. (Default: *sub*)
- enfore_auth: (optional) if set to *True* then Monocle requires a valid JWT to access any API endpoints and the
  WEB Application requires the user to login to navigate. (Default: *False*)

The redirect URI to configure on the OIDC provider is: `<MONOCLE_PUBLIC_URL>/auth/cb`.

Furthermore, two additional settings ([environment variables](#environment-variables)) are required to enable the Authentication provider:

- MONOCLE_PUBLIC_URL: is used to redirect the OIDC provider to the callback endpoint and the user to
  the Monocle WEB Application after a successful authentication.
- MONOCLE_OIDC_<PROVIDER_NAME>_CLIENT_SECRET: Monocle looks for this variable to discover the client secret
  which has been provided by your OIDC provider.

Monocle API generates the following log line when the Authentication provider has been properly set up:

```
2022-10-11 10:39:11 INFO    Monocle.Main:149: AuthSystemReady {"provider":"my provider"}
```

Monocle issues its own JWTs to the WEB Application users. JWTs issued by the OIDC provider are used to get
the authenticated user's information such as its unique uid via the *oidc_user_claim*. Monocle uses the UID (from the OIDC provider) to discover through the [idents](#identity-management) settings a matching
Monocle Ident. A *matching ident* defines an *alias* which is *AuthProviderUID:<user_unique_id>*. In case of
no matching ident then the user claim value is used.

A Monocle JWT is valid for 24 hours, once expired the user must login again to the OIDC provider.

###### Service tokens

Monocle can issue service tokens to be used by applications that consume the Monocle API. To request
a token your need to perform the following call:

```Shell
curl -XPOST -d '{"token": "<admin_token>"}' -H "Content-type: application/json" <monocle_public_url>/auth/get
{"jwt":"eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYURlZmF1bHRNdWlkIjoiYm90IiwiYU11aWRNYXAiOnt9fX0.bmj5vcxXxz2LmkrVKxX8jd-aYzHeTng_QBzR_9YZwCb9ToKA59TVlN1wZf6hhPlUX1VU82Y94gVCREDifintZg"}
```

Then set the *Authorization* header to access Monocle API:

```Shell
curl -XPOST -d '{"void": ""}' -H "Content-type: application/json" -H 'Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYURlZmF1bHRNdWlkIjoiYm90IiwiYU11aWRNYXAiOnt9fX0.bmj5vcxXxz2LmkrVKxX8jd-aYzHeTng_QBzR_9YZwCb9ToKA59TVlN1wZf6hhPlUX1VU82Y94gVCREDifintZg' <monocle_public_url>/auth/whoami
```

The `MONOCLE_ADMIN_TOKEN` must be set to enable this endpoint.

### Full configuration file example

Here are the expected environment variables that need to be added to the `.secrets` file:

- `CRAWLERS_API_KEY`: an arbitrary api key used by the crawler to index data.
- `GITHUB_TOKEN`: an API key for GitHub crawler.
- `GITLAB_TOKEN`: an API key for GitLab crawler.

Open the sample [config.yaml](test/data/config.yaml).

### Troubleshooting

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

To delete a workspace (a workspace is an elasticsearch index):

```ShellSession
# List indexes with:
docker-compose run --rm --no-deps api curl http://elastic:9200/_aliases?pretty=true
# Delete an index with
docker-compose run --rm --no-deps api curl -XDELETE http://elastic:9200/<index-name>
```

ElasticSearch sets defaults settings on new indexes. The default setting for queries based
on regex is set to a value that might not fit your usage especially when your project definitions
uses regex above that limit. However the limit could be increased using the following command:

```ShellSession
docker-compose run --rm --no-deps api curl \
-XPUT http://localhost:9200/monocle.changes.1.<index-name>/_settings \
-H "Content-Type: application/json" -d '{"index": {"max_regex_length": 50000}}'
```

Crawler default ciphers can be restrictive and not able to work with some load balancer of remote
server advertising an unsupported cipher. If you experience `wrong signature type` errors in the
crawler container, you should consider changing ciphers using the `TLS_CIPHER` environment variable
in your docker-compose configuration file. You can find [additional information](https://fedoraproject.org/wiki/Changes/StrongCryptoSettings2) on Fedora changelog.

To disable TLS verification, set the `TLS_NO_VERIFY` environment variable to `1`.


## Wipe crawler data from the database

To wipe any data related to a workspace's crawler:

```bash
docker-compose stop crawler
docker-compose run --rm --no-deps api monocle janitor wipe-crawler-data --elastic elastic:9200 --config /etc/monocle/config.yaml --workspace <workspace> --crawler-name <crawler-name>
docker-compose start crawler
```

## Components

![architecture](./doc/architecture.png)

Monocle is composed of the following services:

1. an Elasticsearch data store.
2. an API service to serve the Web APP and API requests.
3. a crawler service to retrieve change from providers.

The APIs are defined using [protobuf][monocle-protobuf] and served over HTTP through [Monocle OpenAPI][monocle-openapi].

## Consuming metrics via the API

Most of the metrics are exposed via the `api/2/metric/get` endpoint. The `api/2/metric/list` lists the
available metrics. The Monocle Web UI leverages those metrics via the `Catalog` page.

The detail about the query and response format is described in the [Monocle OpenAPI][monocle-openapi] document.

Note that there is an effort in progress to ensure that all metrics displayed by the Monocle Web UI use the
`api/2/metric` endpoint.

To list the available metrics:

```Shell
curl 'https://demo.changemetrics.io/api/2/metric/list' \
  -H 'Content-Type: application/json' \
  --data '{"void":""}'
```

To get the `Changes created` metric as a `trend` with automatic interval:

```Shell
curl 'https://demo.changemetrics.io/api/2/metric/get' \
  -H 'Content-Type: application/json' \
  --data '{"index":"python","username":"","query":"from:now-3weeks","metric":"changes_created","trend":{"interval":""}}'
```

## Monitoring

To setup the monitoring:

1. Create the prometheus service by providing the API and CRAWLER location

```ShellSession
export API_TARGET=localhost:8080
export CRAWLER_TARGET=localhost:9001
mkdir -p /srv/prometheus
podman create --network host -v /srv/prometheus:/var/lib/prometheus:Z -e API_TARGET=${API_TARGET} -e CRAWLER_TARGET=${CRAWLER_TARGET} --name monocle-prometheus quay.io/change-metrics/monocle-prometheus:latest
```

2. Create the grafana service (on the prometheus host)

```ShellSession
mkdir -p /srv/grafana
podman create --network host -v /srv/grafana:/var/lib/grafana:Z -e GRAFANA_PASS=secret --name monocle-grafana quay.io/change-metrics/monocle-grafana:latest
```

3. Starts the services with systemd

```ShellSession
mkdir -p ~/.config/systemd/user/
for service in prometheus grafana; do podman generate systemd -n monocle-$service > ~/.config/systemd/user/monocle-$service.service; done
systemctl --user daemon-reload
for service in prometheus grafana; do systemctl --user start monocle-$service; done
```

4. Check metrics with grafana dashboard at http://localhost:19030/ (login with 'admin:secret')

[monocle-protobuf]: ./schemas/monocle
[monocle-openapi]: ./doc/openapi.yaml

## Users of Monocle

Feel free to open a Pull Request to get listed into this list.

| company name / project name      | main usage (board, metrics, other)      | public instance (url)/private instance        |
|----------------------------------|-----------------------------------------|-----------------------------------------------|
| changemetrics.io                 | project demo instance                   | https://demo.changemetrics.io                 |