# Changelog

All notable changes to this project will be documented in this file.

## [master]

### Added

- [doc] Add a section about metric API usage.
- [crawler] Add a Github user Pull Requests crawler.
- [config] Add an optional `crawlers` section to set global crawlers setting.
- [config] Add a global crawler setting to set the loop delay in second (`loop_delay_sec`).
- [cli] Add a CLI command to stream user Pull Requests.
- [cli] Add a human readable flag to github-changes and github-user-changes commands.
- [cli] Add a command to wipe any data created by a Crawler.

### Changed

- [metric] Changed the Peer strength metric computation to enable filtering by change author (#1037).

### Removed

### Fixed

- [crawler] Authentification token are no longer written in the service logs.

## [1.8.0] - 2022-11-28
### Added

- [api] Add the `file` field to the query language to filter changes changing at least a file path that match the regex.
- [web] Search authors page displays the total count of authors.
- [api] Enable the 'draft' status for Gerrit changes.
- [api] Enable the "draft" state's value in the query language.
- [web] Add the pin change button to highlight a change.
- [web] Add the mask change button to permanently hide a change.

### Changed

- [logging] The api and crawler logs changed to structured json logging.
- [prometheus] The http_request and http_failure counter labels changed from (workspace, url, crawler) to (module, url).

### Fixed

- [crawler] Github crawler no longer crash when rate limit is deactivated on GHE (#958).
- [api] Ordering by task's score (#833).
- [api] Filter on "not author" for top authors metric "By changes reviewed" and "By changes commented".
- [crawler] Support for crawling private repositories on Gitlab (#985).
- [web] A draft change is only displayed as draft when the change is open (#993).

## [1.7.0] - 2022-09-16

### Added

- [api] Add support for the 'ttm' field in the query language. The field allows to filter on merged changes of a specific duration (in seconds).
- [api] Expose the duration field of the change object. Duration is the time to merge delay in seconds.
- [web] A metrics catalog that give access to the base metrics provided by the Monocle API.
- [deployment] Official support of k8s and OpenShift deployment in main README.md.

### Changed

- [web] Keep User-Agent to the URI after an OIDC login to Monocle.
- [nix] The project is now available as an installable flake. The file structure has been updated. Development environment is provided by `nix develop` instead of `nix-shell`.
- [compose] Bumped Elasticsearch to 7.17.5.
- [compose] Changed the image name from `monocle_api` to `monocle`.

### Removed
### Fixed

- [api] The self-merged count on the Activity page was incorrect and it is now properly calculated.
- [crawler] Gerrit and Gitlab crawler compute negative duration for merged changes.

## [1.6.0] - 2022-06-20

### Added

- [cli] github-watched subcommand can be used to list a user's watched projects.
- [api] Authentication support via an OIDC provider (ADR: 0012-OpenID-connect-authentication.md).

### Changed

- [cli/compose] Some environment variables have been renamed.

### Removed

- [compose] Removed the nginx web proxy, the monocle-api now serves the webui.
- [deploy] The MONOCLE_API vars are replaced by MONOCLE_WEB: the monocle-api service listen on port 8080, and monocle no longer uses the ports 989*.

### Fixed

- [web] Web APP title not set properly.

## [1.5.0] - 2022-03-31

### Added

- [crawler] A new env var "TLS_CIPHERS" to override the default OpenSSL cyphers list.
- [webui] A scoped view for Author and Group that features tailored metrics and changes lists.
- [api] Search query QUERY_RATIO_COMMITS_VS_REVIEWS.
- [api] Search queries QUERY_HISTO_COMMITS, QUERY_HISTO_REVIEWS_AND_COMMENTS.
- [api] Endpoint to search author by name.
- [webapp] A page to search author by name.

### Changed

- [api] user_group/get and user_group/get endpoints to get_groups and get_group_members endpoints.
- [webui] Commits/Reviews histo and commits VS reviews ratio moved to the new scoped view components.

### Fixed

- [webui] avoid failure to access a group with url encoded name.
- [api] wrong author filtering for TOP_REVIEWED_AUTHORS and TOP_COMMENTED_AUTHORS queries.
- [crawler] honor the "TLS_NO_VERIFY" environment variable for the Gerrit crawler.
- [backend] Orphan TaskData Doc ID can overflow the Elastic Doc ID size limit (#845).

## [1.4.0] - 2022-01-31

This release mainly removes a technical dept where the GitHub Pull-Requests crawler
was managed differently than other crawlers. With this release, the crawler is managed
by the same process (or container) than other crawlers and respects the same API.
Note this change should be transparent as it does not require changes in the Monocle
configuration or database schema updates.

### Added

- [web] A minimal login capability to enable the use 'self' keyword in queries.
- [cli] --version argument to display the current version

### Changed

- [crawler] Legacy crawler for GitHub Pull Request has been migrated to the new crawler api.
- [crawler] Improved retry logic for graphQL crawler API call errors.

### Removed

- [compose] crawler-legacy container.
- [kube] crawler-legacy container.
- [crawler] support for GitHub APP.

### Fixed

- [index] fix missing schema info for commit.committer fields.

## [1.3.0] - 2021-12-02

### Added

- [config] Ability to define custom links to be displayed in the web UI.
- [web] An about modal that displays configured links and Monocle version.
- [web] A new toggle can be used to show or hide change until they are updated.
- [web] Board - swap button for board columns editor.
- [monitoring] prometheus metrics and instructions to start with prometheus and grafana containers.

### Changed

- [web] Left side menu can be collapsed/expanded.
- [web] Search field moved out of the top menu into a Patternfly PageSection.
- [cli] Interface to run update-idents command.

### Removed

- [backend] The `monocle-api` and `macroscope` CLI have been merged into a single `monocle` tool.

### Fixed

- [api] workspaces' crawler entities not created/refreshed at config changes [#739](https://github.com/change-metrics/monocle/pull/739).

## [1.2.1] - 2021-10-28

### Fixed

- [crawler] Identities aliases are now properly assigned for GitLab and Gerrit author.
- [crawler] Gerrit - mergeable status not correctly computed.
- [crawler] Gerrit - ChangeCommentedEvent and ChangeReviewedEvent overlap.
- [crawler] Gerrit - Fix decoding issue where a Gerrit author name might be undefined.

## [1.2.0] - 2021-10-26

Notice that the crawler and api containers are based on a recent version of Fedora 35 that
requires a recent Docker (>= 20.10.3).

### Added

- [contrib] Example of Python clients for the Monocle OpenAPI.
- [web] filter by labels.

### Changed

- [crawler] Gerrit crawler is managed by Macroscope and removed from crawler-legacy.
- [api/web] Some keys name in the Monocle Query Language (eg. all task related keys are prefixed with "task.")

### Removed

- [api] The legacy task data last_updated/add/commit are replaced by the new crawler commit_info/add/commit api.

### Fixed

- [crawler] GitHub issues are now collected for all projects, (it was only fetching the first one).

## [1.1.0] - 2021-09-21

### Added

- [macroscope] Added support for BugZilla crawler.
- [macroscope] Added support for GitHub Issues crawler.
- [config] Support update_since date format as "YYYY-mm-dd" for macroscope crawlers.
- [web] Added suggestions in Fields selector for projects and groups.
- [web] Display Task Data info in Board Kanban view.
- [web] Make the search help a modal.
- [web] Display a spinner when the query is refreshing.
- [web] Total open changes on repository summary page.

### Changed

- [web] Only display mergeable status if conflicting.
- [web] Board - add column for branch and make it clickable
- [web] Display "Abandoned" is state instead of "closed" and do not display mergeable status.
- [web] Board - improve editor layout.
- [web] Make change title links to the external change page.
- [web] Adjust the default board to show Done at the end.
- [web] Adjust changes view to display a maximum of 256 changes.
- [web] Nomenclature of columns in Repository view.
- [api] repo, branch and author are now regex query
- [api] task query are now named tag and it is a regex

### Removed

- [api] Support for unicode operators and whitespace around non boolean operators in the query language.
- [web] Footer stickyness.

### Fixed

- [api] Peer strengh and repo summary query slowness has been reduced.

## [1.0.0] - 2021-08-17

This release features some major refactoring. See [v1-roadmap](https://changemetrics.io/posts/2021-07-06-v1-roadmap.html).
Most notable changes are:

- a new [OpenAPI](./doc/openapi.yaml) served through the Haskell RunTime
- a new WEB application written in ReScript
- a new query system based on the Monocle query language

### Added

- [api,web] Query Language to replace the filter form.
- [crawler] Support GitLab provider.

### Changed

- [web] Multiple UX improvements.

### Removed

- [api] Index acl authentication using the `users` settings is removed. To protect an index use an authentication proxy instead.
- [web] Remove support of the FilterForm based query system.

### Upgrade

- [config] The crawler configuration is now using a new schema, use the `monocle migrate-config` command to upgrade the configuration.
- [config] The secret are now passed through environment variables, use the new `.secrets` file with docker-compose.

## [0.9.0] - 2021-05-24

### Added

- [web] Support for REMOTE_USER header to enable external authentication system
- [config] Added configuration section to enable the merge of contributor's metrics
  across code review systems.
- [dbmanage] Added update-idents option.
- [api,web,crawler] - added secure communication (SSL) and user authentication with Elasticsearch
- [api] Added the `api/1/get_projects` endpoint (list defined project for an index).
- [api] Added the query parameter `project` to query metrics for a given project.
- [web] Add a project filter selection in the Filter Box.
- [api,config] Endpoints to connect task tracker crawlers
- [api] Support of three new query attributes (task_type, task_priority, task_severity)
- [compose] Added ADDR and PORT variables to enable multiple service deployment on a single host
- [compose] Simplified deployment by using a single MONOCLE_PUBLIC_URL for both monocle-web and monocle-api using nginx proxy pass.
- [web] Added suggestion in filter box for approval, authors and task data type, severity, priority fields
- [api,web] Added task data score support in the api and the web ui (filterform)

### Changed

- [web] Change state filter becomes a multi-term field
- [web] change terminology from "average to" to "mean time to".
- [compose] Renamed MONOCLE_URL into MONOCLE_PUBLIC_URL
- [compose] Replaced the node http serve with nginx

### Removed

- [compose] Replaced MONOCLE_API_URL with MONOCLE_API_ADDR and MONOCLE_API_PORT

### Fixed

- [config] Add missing validation for `prefix` option of the Gerrit crawler.
- [crawler] Add missing change url field for gerrit events

### Upgrade

- [readme] Added migration instructions for identity consolidation.
- [dbmanage] Added migration process: from-0.8-to-last-stable

## [0.8.1] - 2021-02-15

### Fixed

- [web] Fix links computation on the repositories summary page.

## [0.8.0] - 2021-02-12

### Added

- [crawler] compute if a change is self merged.
- [api] add the self_merged query parameter.
- [web] Add ui integration for the self-merged changed.
- [web] compute and display self_merged/created ratio.
- [docker-compose] added restart and health-check directives for api and elastic.
- [crawler] add an optional prefix for gerrit projects (configured in
  the config file).
- [crawler] add a dummy driver for documentation purpose.
- [web] Add a direct link to external change url in changes pages.
- [web] Add links to changes page from the repositories summary page.
- [api] changes_lifecycle_stats return the Median Deviation of duration (Time To Merge).
- [web] Display the "Median Deviation of TTM".

### Changed

- [api] bump Python version to 3.9.1.
- [database] bump Elasticsearch to 7.10.1.

### Removed

### Fixed

- [api] Ensure the gte and lte date filters are set after all other params are set.

### Upgrade

- [db] added the run-migrate option to dbmanage to run migration scripts.
- [readme] migration instructions for the new self_merged field has added.

## [0.7.0] - 2020-08-28

### Added

- [web]: display a filter summary below the filter box.
- [api] and [web]: add a repos summary page

### Changed

- [db] set max_buckets to a higher default value
- [web] and [api]: better display of api error and logging server side
- [web]: re-organization of changes related pages.
- [web]: re-factored top menu.
- [web]: make approval pie clickable.
- [web]: improve relative date selector.

### Removed

### Fixed

- [web] pie charts invisible when legend is too large.

## [0.6] - 2020-08-20

### Added

- [cli] add dbmanage option to delete a Monocle index.
- [web] add the approvals and exclude_approvals filters available into the changes pages.
- [api] add the exclude_approvals paramater support.
- [crawler] gerrit add support for http basic auth.
- [crawler] gerrit add insecure option to bypass SSL certificate verification.

### Changed

- [database] switch to ElasticSearch from 6.8 to 7.8 engine. Indexes created
  with previous Monocle version are compatible.
- [web] better responsive layout for the filtersbox.
- [api] rename param approval to approvals and make it comma separated list.
- [web] reworked the look and feel of changes tables for a better readability
- [web] set bootstrap table with size=sm for a better look and feel
- [crawler] github application: use authlib.jose.jwt instead of pyjwt.

### Removed

### Fixed

- [compose] change the selinux label Z to z. This indicates that the bind
  mount content is shared among multiple containers". This fixes an issue where
  the config file is not found in the in crawler or api container.
- [api] validate updated_since date format in the configuration file.
- [api] fix tz issue in queries due to the use of naive datetime object.
- [api] ensure most_active_authors_stats display top authors by merged change.
- [crawler] gerrit ChangeReviewedEvent approval remove leading space.
  No data migration script provided, a wipe then re-indexation of the
  gerrit repositories is needed.
- [api] fix change_lifecycle_stats return null values when authors is set
- [web] fix wrong backend query on the change page (gte missing)

## [0.5] - 2020-06-04

### Added

- [crawler] support for a GitHub application.
- [web] warning message when small screen (width < 1024).
- [web] display active authors histogram in People page.
- [api] add authors_histo and authors_histo_stats queries.

### Changed

- [crawler] adaptative page size for GitHub. If it's not enough, don't
  get the commits data when we are failing for one PR.
- [web] moved approval dispersion pie in changes summary.

### Removed

### Fixed

- [crawler] protect get rate limit calls.
- [web] better look and feel on small device like smartphone.
- [api] limit first_comment|review_on_changes to 10000 objects to avoid performance issue.

## [0.4] - 2020-05-11

### Added

- [compose] expose ES_XMS and ES_XMX to configure the ElasticSearch JVM HEAP size.
- [compose] add MONOCLE_API_URL env var to ease production deployment.
- [web] add a footer with a link to the GitHub project.
- [web] allow to customize the title with the `REACT_APP_TITLE`
  environment variable or `MONOCLE_TITLE` in the `.env` file.
- [web] do not display login link if authentication is not configured.
- [api] use Flask-Caching to cache requests for 5 minutes.
- [web,api] compute and display pie charts for the abandoned-changes page.
- [web] make the authors and repos pie charts clickable.
- [api] add the has-issue-tracker-links parameter.
- [api] add the tests_included parameter.
- [install] split `README.md` in [`README.md`](README.md) and [`CONTRIBUTING.md`](CONTRIBUTING.md).
- [build] build containers in 2 steps to save space.

### Changed

- [crawler] github use exponential tenacity retry
- [crawler] follow [GitHub abuse rate limits guidelines](https://developer.github.com/v3/guides/best-practices-for-integrators/#dealing-with-abuse-rate-limits):
  obey to the Retry-After header for the GitHub api calls, sleep 1s
  between calls and set the User-Agent header to change-metrics/monocle.
- [compose] allow to configure on which addresses the services are exposed.
- [web] fix the menu header.
- [web] add a spinner in the LoadingBox.
- [web] put the new contributors at the end of the /people/ page because the list can be long.
- [web] make the peer strength graph smaller on the /people/ page.
- [crawler] log error before retrying GitHub graphql queries.
- [backend] upgraded to Elasticsearch 6.8.8.
- [web, api] improve and move issue tracker links detection from ui to the backend.
- [web] add a new favicon.ico.
- [web] split top page into a people and changes pages.
- [dev] distribute 2 docker-compose.yml files: one for dev and one using images from docker hub.

### Removed

- [web] do not use the Treemap anymore as it is not consistent in the changes view.

### Fixed

- [api] fix buggy ratio when a change is closed then re-opened
- [web] fix fetching data when changing page.
- [web] do not recreate menu and filter form if not needed.
- [api] fix wrong http code return in case of accessing unauthorized index.
- [web] api calls perfomed multiple times - history listener was not unregistered.
- [crawler] sleep 120 s when we have this message from GitHub: 'You
  have triggered an abuse detection mechanism. Please wait a few
  minutes before you try again.'.
- [crawler] fix GitHub PR with empty files.
- [web] carry search params in links inside complexity graphs.
- [web] sync between exclude_authors search param and filter field.
- [web] passing `REACT_APP_API_URL` in container image mode.

## [0.3] - 2020-05-01

### Added

- [crawlers] skip GitHub archived repositories.
- [web] list all indices on home page.
- [web] allow to filter on files (regexp).
- [web] add approvals in open changes listing.
- [db] add approvals in Change object for Gerrit and Github crawlers.
- [web] display the timeline of commits in /change/.
- [web,api] add github authentication and basic authorization engine
- [web,api] allow to filter on target_branch. Display the branch name of a change in /change/.
- [web,api] display mean time to merge and commits per change in lifecycle stats.
- [web] add a homepage redirecting to the first index or displaying a message to create an index.
- [api] add a /indices api to get the list of available indices.
- [web] add a Treemap of the files impacted by the changes on the opened and merged pages.
- [api] add 2 requests: changes_by_file_map and authors_by_file_map.
- [api] add files (regexp) and state (`OPEN`, `CLOSED` or `MERGED`) as query parameters.

### Changed

- [api,backend] renamed projects.yaml to config.yaml. projects key replaced by tenants key. [BREAKING]
- [db] add target_branch and branch to events. Need to re-index to be able to use these fields. [BREAKING]
- [db] prefix the names of indices by `monocle.changes.`. Need to recreate the indexes or create an alias if you want to keep them. [BREAKING]

### Fixed

- [db] fix missing ChangeReviewedEvent events for Gerrit crawler
- [web] back and forward browser buttons are working fine now when you change filters.
- [web] have a correct history of events in /change/ when filtered on authors (#136)

## [0.2] - 2020-04-24

### Added

- [dev] document Architectural Decisions in [doc/adr/index.md](doc/adr/index.md)
- [web,backend] add support for Github Draft status in PR. Displayed
  in change details and tables of open changes.
- [web] display mergeable status in open changes.
- [web] add a relative date selection box in the filter form.
- [dev] functional tests for the crawler and the query system.
- [web] menus to ease navigation.
- [web] a page to display details about a change.
- [web] sub pages to be able to explore the complete list of changes that are merged, opened or abandonned.
- [web] enhanced navigation by having links changing the filters automatically on users and repositories.
- [web] chord diagram to display collaboration.
- [web] expose the complexity of a change: number of added lines + number of removed lines + number of impacted files.
- [dev] use eslint for js code and black for python code.
- [admin] add kibana support using docker-compose extra file in the contrib directory.
- [dev] [hacking instructions in README.md](README.md#hacking).

### Changed

- [web] make the browser back button work if the search filter changes
- [backend] crawler uses one thread per Github token.
- [backend] crawler retries multiple times if a Github query doesn't work.
- [web] interval is no more exposed in the UI and computed automcatically in the backend according to the period.
- [web] humanize dates and durations.
- [backend] store the filenames impacted by a change. Need to recreate the indexes.

### Fixed

- [backend] peer strength computation.
- [backend] crawler dumps a PR or review in the dump directory if it cannot parse it.
- [web] no more warnings on the console output.
- [backend] managed Github deleted accounts (ghost).

## [0.1] - 2020-04-01

- initial release
