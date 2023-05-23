# Deploy Monocle on k8s

Note that the following procedure must be valid for deploying on OpenShift. For this
you might want to use the `oc` command instead of `kubectl`.

Alternatively to this process, a k8s operator is available on [operatorhub.io](https://operatorhub.io/operator/monocle-operator).

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
kubectl apply -f k8s/
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

Create a `config.yaml` file on your local filesystem. Here is a sample one:

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

Then update the `monocle-config` ConfigMap with:

```
kubectl create configmap monocle-config --from-file config.yaml -o yaml --dry-run=client | kubectl replace -f -
```

Monocle api and crawler processes will detect the config change and reload the
configuration automatically.

If a secret needs to set for a crawler, then the `monocle-secrets` secret resources must be updated
and the crawler deployment must be re-created. To do so, amend your `.secrets` file to add the new
environment variable then run:

```
kubectl create secret generic monocle-secrets --from-file .secrets -o yaml --dry-run=client | kubectl replace -f -
kubectl delete deployment crawler
kubectl apply -f k8s/crawler-deployment.yaml
```

## Run the update-idents janitor

To update author identities, the operator needs to join the Monocle api Pod via kubectl exec:

```
kubectl exec -it api-9dfb77ddd-5xtvt bash
monocle janitor update-idents --config /etc/monocle/config.yaml --elastic elastic:9200
```
