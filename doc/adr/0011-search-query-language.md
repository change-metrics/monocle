# 11. Search query language

* Date: 2021-05-19

## Context and Problem Statement

To build custom dashboards we need to define queries that are too complex for the existing filter box form.
We would like to use flexible search expressions based on a proper query language.

## Considered Options

* GitHub Search Syntax
* Kibana Query Language
* Luqum
* Monocle Query Language

## Decision Outcome

Chosen option: "Monocle Query Language", because it comes out best (see below).

### Positive Consequences

- We improve the user experience by replacing the clunky filter box with a simpler search bar.
- We create a re-usable component.

### Negative Consequences

- We need to maintain a language toolchain.

## Pros and Cons of the Options

### GitHub Search Syntax

https://docs.github.com/en/github/searching-for-information-on-github/getting-started-with-searching-on-github/understanding-the-search-syntax

* Good, because many parser already exists.
* Bad, because it does not support boolean operators.

### Kibana Query Language

The Kibana Query Language (KQL) is a simple syntax for filtering Elasticsearch data using free text search or field-based search:
https://www.elastic.co/guide/en/kibana/current/kuery-query.html

* Good, because it is standard in the elastic stack.
* Bad, because it is complex to integrate in the existing code base.

### Luqum

Luqum stands for LUcene QUery Manipolator: https://luqum.readthedocs.io

* Good, because it is simple to use.
* Bad, because it is not supported on the client side.
* Bad, because it supports complex syntax.

### Monocle Query Language

We implement our own query language:

- As a subset of KQL, e.g. without generic nested field.
  Instead of `change.reviewer.{ author: foo }` we provide a flat `change_reviewer_author: foo` field.
- Server side validation and es query transformer, e.g. to expand group name fields.

* Good, because it fits our domain.
* Bad, because it needs to be implemented.
