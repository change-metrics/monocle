# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- [install] split `README.md` in [`README.md`](README.md) and [`CONTRIBUTING.md`](CONTRIBUTING.md).
- [build] build containers in 2 steps to save space.

### Changed

- [dev] distribute 2 docker-compose.yml files: one for dev and one using images from docker hub.

### Removed
### Fixed

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
