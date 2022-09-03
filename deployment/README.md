# Deploy Monocle on K8S

Note that the following procedure must be valid for deploying on OpenShift. For this
you might want to use the `oc` command instead of `kubectl`.

## Initial deployment

The `secrets` file must be populated with at least the `CRAWLERS_API_KEY`.

```
echo CRAWLERS_API_KEY=$(uuidgen) > .secrets
```

And expose the `secrets` file as a kubernetes secret resource. It will be mounted
as a volume on the api and crawler deployments.

```
kubectl create secret generic monocle-secrets --from-file=.secrets
```

Then apply manifests:

```
kubectl apply -f deployment/
```

You can then access the Monocle Web UI by setting a port forward

```
kubectl port-forward service/api 8080:8080
firefox http://localhost:8080
```

You might want to setup an HTTP Ingress instead of the port forward if your cluster support it.

## Update Monocle configuration

The inital deployment sets an empty config file via a ConfigMap mounted as a volume. To
amend the configuration we need to:

Create a config.yaml file on your local filesystem. Here is a sample one:

```
workspaces:
  - name: demo
    crawlers:
      - name: zuul-opendev
        provider:
          gerrit_url: https://review.opendev.org
          gerrit_repositories:
            - ^zuul/zuul
        update_since: '2022-07-01'
```

Then update the ConfigMap with:

```
kubectl create configmap monocle-config --from-file config.yaml -o yaml --dry-run=client | kubectl replace -f -
```

Monocle api and crawler processes will detect the config change and reload the
configuration automatically.

If a secret needs to set for a crawler, then the monocle-secrets secret resources must be updated
and the crawler deployment must be re-created.