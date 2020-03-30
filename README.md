# Monocle

Monocle purpose is to provide Pull Requests and Reviews metrics through a web API and a web UI. Monocle supports Github and Gerrit. Monocle provides a set of crawlers designed to fetch Pull Requests and Reviews data from the Github or Gerrit APIs and to store changes and changes related events into Elasticsearch. Furthermore Monocle implements ready to use queries and a React based web UI.

## Deploy the master version

Monocle is an early phase of developement. The process below will help you to index Pull Requests events of a Github organization and to start the web UI to browse metrics using `docker-compose`.

### Clone and prepare data directory

```Shell
git clone https://github.com/morucci/monocle.git
cd monocle
mkdir data
```

### Create the github-env file

Generate a personal access token on Github (w/o any specific rights).

Then create the config file `github-env`:

```Shell
GITHUB_TOKEN=<your token>
GITHUB_ORG=<the github org>
GITHUB_UPDATED_SINCE=<start date YYYY-MM-DD>
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

### Accessing the web UI

You should be able to access the web UI at http://localhost:3000/.
