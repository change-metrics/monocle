# Monocle

Monocle purpose is to provide metrics about Changes through a web API and a web UI. Monocle supports Github Pull Requests and Gerrit Reviews. Monocle provides a set of crawlers designed to fetch Pull Requests and Reviews data from the Github or Gerrit APIs and to store changes and changes related events into Elasticsearch. Furthermore Monocle implements ready to use queries and a React based web UI.

![Screenshot](https://user-images.githubusercontent.com/529708/78028243-fc958980-735e-11ea-8fd4-f5ecfb6af02a.png)

## Deploy the master version

Monocle is in an early phase of developement. Feedback is highly welcome.

The process below describes how to index changes from a Github repository, a full Github organisation and Gerrit repositories, and then how to start the web UI to browse metrics using `docker-compose`.

### Clone and prepare data directory

```Shell
git clone https://github.com/morucci/monocle.git
cd monocle
mkdir data etc
```

### Create the github-env file

Generate a personal access token on Github (w/o any specific rights).

Then create the config file `etc/projects.yaml`:

```YAML
---
projects:
  - index: monocle
    crawler:
      loop_delay: 10
      github_orgs:
        - name: tektoncd
          repository: pipeline
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
        - name: spinnaker
          updated_since: "2020-03-15"
          token: <github_token>
          base_url: https://github.com
      gerrit_repositories:
        - name: ^zuul/.*
          updated_since: "2020-03-15 00:00:00"
          base_url: https://review.opendev.org
```

### Start docker-compose

```Shell
docker-compose up -d
```

ElasticSearch could need some capabilities to run in container
mode. Take a look at the logs to see if it started correctly:

```Shell
docker-compose logs elastic
```

For example, you could need to increase this system parameter:

```Shell
sudo sysctl -w vm.max_map_count=262144
```

or make the data directory writable for other:

```Shell
chmod o+w data
```

You might need to check the crawler logs:

```Shell
docker-compose logs crawler
```

### Accessing the web UI

You should be able to access the web UI at <http://localhost:3000/monocle>.

## Hacking

This section explains how you can hack the Monocle code. The idea is to use
the docker deployment to avoid complex development methods.

After changes, simply run the following command. docker-compose will
rebuild and re-spawn the changed containers:

```Shell
docker-compose up -d --build
```

### Git hooks

### pre-push

To be sure to push correct branches, you have to configure the
`pre-push` git hook by creating `.git/hooks/pre-push` with the
following content:

```Shell
#!/bin/bash

exec ./contrib/pre-push "$@"
```

and making it executable with "chmod +x .git/hooks/pre-push".

### pre-commit

Optionnaly, you can enable the `pre-commit` git hook to reformat your
python code by creating `.git/hooks/pre-commit` with the following
content:

```Shell
#!/bin/bash

exec ./contrib/pre-commit "$@"
```

and making it executable with "chmod +x .git/hooks/pre-commit".
