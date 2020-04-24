# 4. High Level Components

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2019-12-02

## Context and Problem Statement

We want to start with a set of components each dedicated to a task to ensure Monocle being able to scale and also to fit well in a cloud like deployment. No monolithic app, but microservices. As we start we also don't want to overengineer, so just keep the basic components and architecture.

## Considered Options

* A database, a crawler, a, CLI, a WEB API, a WEB UI.

## Decision Outcome

Choosen Option: "A database, a crawler, a, CLI, a WEB API, a WEB UI".
Because, it fits the described context. The database (Elasticsearch) is scalable, and the other components are stateless, each of them use the database or the API as backend so this will ease in term of scalability. This architecture makes it easy to deploy using docker/podman or even k8s.
