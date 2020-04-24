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
