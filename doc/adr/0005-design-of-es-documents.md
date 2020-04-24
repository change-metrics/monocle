# 5. Design of ES documents

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2019-12-02

## Context and Problem Statement

We want to support most of the existing Code Review systems. Monocle queries should be agnostic regarding the data source.

## Considered Options

* A crawler by data source and an unique and generic data schema.

## Decision Outcome

Choosen option: "A crawler by data source and an unique and generic data schema".
Because, the addition of a data source support must not have any impact on the queries, the CLI or the WEB UI. The DB schema will be complete enough to fill the basic requirements of each source but will not implement specificities. The terminilogy will be generic for instance a Pull Request and a Review (Gerrit) will be called a "Change".

### Database objects

* Change: an object that describe the status of a Pull Request or Gerrit Review. The Change object will be attached attributes such as: the creation data, the author, the amount of commits within the Change, the changed files list, the title, ...
* ChangeCreatedEvent: an object that describe the creation event of a Change
* ChangeCommentedEvent: an object that describe a comment event on a given Change
* ChangeReviewedEvent: an object that describe a review event on a given Change
* ChangeCommitPushedEvent: an object that describe a commit push event on a given Change
* ChangeCommitForcePushedEvent: an object that describe a commit force push event on a given Change
* ChangeMergedEvent: an object that describe a Change merge event
* ChangeAbandonedEvent: an object that describe a Change abandoned event
