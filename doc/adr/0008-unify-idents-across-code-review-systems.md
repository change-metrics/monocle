# 8. Unify idents across code review systems

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2021-03-11

## Context and Problem Statement

How do we merge contributor identities across code review systems ?

## Decision Drivers

* Do not complexify the EL query system
* Identities can be configured via the Monocle configuration file

## Considered Options

* [option 1] Identities are described by the Monocle operator into the configuration file.
  Queries result is amended to group/merge returned objects based on identities definitions.
  EL Query builder must check identities definitions and adds OR clause on all fields related
  to authorship like authors/committers/.... 
* [option 2] Identities are described by the Monocle operator into the configuration file.
  The operator runs a tool to update the EL database object to update authorship fields.
  No change needed on the query system.

## Decision Outcome

Chosen option: "[option 2]", Simpler implementation, better query performance (no complexification of queries) and no potential metrics errorneous results returned compared
to "[option 1]".

Here is a configuration example.

```
idents:
  - ident: John Doe
    aliases:
      - github.com/john-doe
      - review.opendev.org/John Doe/12345
```


### Negative Consequences

* A tool need to crawl the whole index to discover objects to update. However this
  operation should not happen often.
