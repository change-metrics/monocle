.PHONY: dependencies static_analysis

dependencies:
	pip3 install --upgrade pip
	pip3 install -r tests/requirements.txt

static_analysis: dependencies
	tox -r -e linters
