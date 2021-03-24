#!/bin/sh

# FIXME: Remove --insecure option for checking TLS in the future.
#        Also change admin:admin credentials.
monocle --elastic-conn elastic:9200 \
        --use-ssl \
        --insecure \
        crawler --config /etc/monocle/config.yaml
