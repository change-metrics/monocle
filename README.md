# Monocle

## Work in progress

## Install

```Shell
git clone https://github.com/morucci/monocle.git

cd monocle

mkdir data

docker pull docker.elastic.co/elasticsearch/elasticsearch:6.8.7

docker run -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" --mount type=bind,source=$PWD/data/,destination=/usr/share/elasticsearch/data docker.elastic.co/elasticsearch/elasticsearch:6.8.7

virtualenv .venv

. .venv/bin/activate

pip install -r requirements.txt

python setup.py develop

ORG=<your org>
TOKEN=<your token>

monocle github_crawler --host github.com --token $TOKEN --org $ORG --updated-since 2020-03-01T00:00:00Z --loop-delay 10 &

# to test if something has been indexed

monocle dbquery --name count_authors  --repository "^.*"

python monocle/webapp.py &

cd web

yarnpkg
yarnpkg start
```
