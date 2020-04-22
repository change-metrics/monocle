# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

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

- [backend] crawler uses one thread per Github token.
- [backend] crawler retries multiple times if a Github query doesn't work.
- [web] interval is no more exposed in the UI and computed automcatically in the backend according to the period.
- [web] humanize dates and durations.
- [backend] store the filenames impacted by a change. Need to recreate the indexes.

### Removed

### Fixed

- [backend] peer strength computation.
- [backend] crawler dumps a PR or review in the dump directory if it cannot parse it.
- [web] no more warnings on the console output.
- [backend] managed Github deleted accounts (ghost).

## [0.1] - 2029-04-01

- initial release
