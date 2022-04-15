# 12. OpenID Connect Authentication

* Status: accepted
* Date: 2022-04-13
* Deciders: Fabien Boucher
* Issue: https://github.com/change-metrics/monocle/issues/868

## Context and Problem Statement

Monocle needs a way to authenticate and authorize users on its GUI and when using its API.
An industry standard should be chosen to make Monocle easily pluggable to existing IAMs.

## Decision Drivers

* An industry standard should be favored for obvious reasons.
* Adding authentication should have a minimal impact on the existing code base.

## Considered Options

* OpenID Connect
* SAML

## Decision Outcome

Chosen option: "OpenID Connect" as a more modern, easier to use and implement solution.

### Positive Consequences

* True authentication in Monocle.
* Support for access control.

### Negative Consequences

* Generating a JWT is not always trivial for direct API use.
* The API will need to handle CORS if it is not already the case.
* Some social login providers (GitHub, ...) do not support OpenID Connect out of the box
  and may require an intermediary service like Keycloak to be usable.

## Pros and Cons of the Options

### OpenID Connect

* Good, because it is an industry standard.
* Good, because JWTs are easy to use and debug.
* Bad, because OpenID Connect is meant for web workflows, and is not easy to use with CLIs.

### SAML

* Good, because it is an industry standard.
* Bad, because SAML is hard to use and debug (XML-based).
* Bad, because SAML is not easy to use with CLIs.
