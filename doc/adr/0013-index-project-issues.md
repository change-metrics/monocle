# 13. Index issues along with changes

* Status: proposed

## Context and Problem Statement

* Issues are an important metric to capture and represent.
* Presently, monocle only collects the issues that are associated to a change using the "task data" feature.
* Because task data are nested, they can't be queries like change event, for example to display the issue activity.

## Considered Options

* Add a new Issue and IssueEvents documents
