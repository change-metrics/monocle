# Monocle

Monocle purpose is to provide Pull Requests and Reviews metrics through a web API and a web UI. Monocle supports Github and Gerrit. Monocle provides a set of crawlers designed to fetch Pull Requests and Reviews data from the Github or Gerrit APIs and to store changes and changes related events into Elasticsearch. Furthermore Monocle implements ready to use queries and a React based web UI.

## Deploy the master version

Monocle is an early phase of developement. The process below will help you to index Pull Requests events of a Github organization and to start the web UI to browse metrics.

### Clone and prepare data directory

```Shell
git clone https://github.com/morucci/monocle.git
cd monocle
mkdir data
```

### Start Elasticsearch

#### For Docker >= 17.06

```Shell
docker run -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" --mount type=bind,source=$PWD/data/,destination=/usr/share/elasticsearch/data docker.elastic.co/elasticsearch/elasticsearch:6.8.7
```

#### For Docker < 17.06

```Shell
chmod o+w data
docker run -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" -v $PWD/data/:/usr/share/elasticsearch/data:Z docker.elastic.co/elasticsearch/elasticsearch:6.8.7
```

### Install Monocle

```Shell
virtualenv .venv
source .venv/bin/activate
pip install -r requirements.txt
python setup.py develop
```

### Run the Github crawler

```Shell
ORG=<your org>
# Generate a personal access token on Github (w/o any specific rights)
TOKEN=<your token>
monocle github_crawler --host github.com --token $TOKEN --org $ORG --updated-since 2020-03-01
```

### Validate the crawler indexed data

```Shell
monocle dbquery --name count_authors  --repository "^.*"
```

Output should be at least 1.

### Start the web API

```Shell
python monocle/webapp.py
```

### Run the web UI

```Shell
cd web
npm i && npm start
```
