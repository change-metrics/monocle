# 9. Changes extended with task data

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2021-04-20

Technical Story: https://github.com/change-metrics/monocle/issues/301

## Context and Problem Statement

As a user, I want to get Changes metrics related to tasks defined
in a task tracker. A simple usecase example is to get insight of the
ratio of changes related to Feature Requests vs Bug fixing.

## Decision Drivers

* Simple implementation
* No assumption about the tasks tracker
* Support of a set of generic fields related to a task

## Considered Options

* Monocle API provides an interface for external task data crawlers
* Monocle provides crawlers for most popular task trackers

## Decision Outcome

Chosen option: "Monocle API provides an interface for external task data crawlers", because it will ease integration between Monocle and the
various task trackers available in the market.

A Monocle operator will need to write the crawler for its
own task tracker. However, over the time, operators might have written crawlers for most popular systems and released them under a open source license.

From the Monocle side, we provide a clear API for a task tracker
crawler to push task related data to Monocle. Each task data sent to Monocle must at least set a predifined set of generic attributes like
"severity", "change_url", "title", ... The monocle API is then able to
find corresponding Changes in the database that match the "change_url" field.

Each changes get a new attribute called "task_data" that is a list of
related task data records. Indeed multiple tasks might be related to a
single change.

Also, Monocle keeps track of task data that do not match any Changes in the
database. The Monocle change crawlers engine triggers a search for adoption of orphaned task data.


