# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [master]

### Added

- [crawler] compute if a change is self merged.
- [api] add the self_merged query parameter.
- [web] Add ui integration for the self-merged changed.
- [web] compute and display self_merged/created ratio.

### Changed
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

##  [0.4] - 2020-05-11

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

##  [0.3] - 2020-05-01

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
