#!/bin/sh

auth=" --user ${ELASTIC_USER:-'admin'}:${ELASTIC_PASSWORD:-'admin'}"
# FIXME: Remove -k for checking TLS verification. Also remove admin:admin credentials.
curl -k --silent $auth --fail localhost:9200/_cluster/health
