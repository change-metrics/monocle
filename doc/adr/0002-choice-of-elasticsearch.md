# 2. Choice of Elasticsearch

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2019-12-02

## Context and Problem Statement

We need to store changes data (Pull Requests and Reviewes) in a scalable way. The stored data must be easily accessible in an intelligible manner.

## Considered Options

* MySQL
* ElasticSearch

## Decision Outcome

Chosen option: "ElasticSearch".
Because it fits better our need regarding the style of data we expect to store and how we expect to query the data.

## Pros and Cons of the Options

### MySQL

* Good, because free and well known interaction format SQL

### ElasticSearch

* Good, because it is a search engine (full text)
* Good, because it implements boolean filters and powerful aggregations
* Good, because it is designed by default to scale
* Good, because it is fast, performance is not an issue even with million docs
* Good, bacause our data can be stored denormalized and ElasticSearch fits this usecase
* Bad, because open core and some features might not be available for free

