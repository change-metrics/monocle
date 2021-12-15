# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later
.PHONY: up-stage

MESSAGES = monocle/user_group.proto monocle/search.proto monocle/config.proto monocle/login.proto monocle/metric.proto
CRAWLER = monocle/change.proto monocle/crawler.proto
BACKEND_ONLY = monocle/project.proto
PINCLUDE = -I /usr/include $(PROTOC_FLAGS) -I ./protos/

codegen: codegen-ci codegen-python codegen-javascript codegen-stubs codegen-openapi codegen-haskell doc/architecture.png

codegen-ci: .github/workflows/nix.yaml
.github/workflows/nix.yaml: .github/workflows/ci.dhall .github/workflows/mkCI.dhall
	echo "(./.github/workflows/ci.dhall).Nix" | dhall-to-yaml > .github/workflows/nix.yaml

doc/architecture.png: doc/architecture.plantuml
	plantuml ./doc/architecture.plantuml

codegen-stubs:
	mkdir -p srcgen/
	(cd codegen; cabal run monocle-codegen ../protos/monocle/http.proto ../haskell/src/Monocle/Client/Api.hs ../haskell/src/Monocle/Servant/HTTP.hs ../monocle/webapi.py ../srcgen/WebApi.res)
	ormolu -i ./haskell/src/Monocle/Client/Api.hs ./haskell/src/Monocle/Servant/HTTP.hs
	black ./monocle/webapi.py
	./web/node_modules/.bin/bsc -format ./srcgen/WebApi.res > ./web/src/components/WebApi.res
	rm -Rf srcgen/

codegen-haskell:
	sh -c 'for pb in $(MESSAGES) $(CRAWLER) $(BACKEND_ONLY); do compile-proto-file --includeDir /usr/include --includeDir protos/ --includeDir ${PROTOBUF_SRC} --proto $${pb} --out haskell/codegen/; done'
	find haskell/codegen/ -type f -name "*.hs" -exec sed -i {} -e '1i{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}' \;
	find haskell/codegen/ -type f -name "*.hs" -exec ormolu -i {} \;

codegen-python:
	protoc $(PINCLUDE) --python_out=./ --mypy_out=./ $(MESSAGES) $(CRAWLER)
	black monocle/*.py*

codegen-javascript:
	rm -f web/src/messages/*
	sh -c 'for pb in $(MESSAGES); do ocaml-protoc $(PINCLUDE) -bs -ml_out web/src/messages/ protos/$${pb}; done'
	python3 ./codegen/rename_bs_module.py ./web/src/messages/

codegen-openapi:
	protoc $(PINCLUDE) --openapi_out=./doc/ monocle/http.proto
	@echo Created doc/openapi.yaml

codegen-with-container:
	podman run -it -v $(shell pwd):/data:z --rm changemetrics/monocle_codegen make
	@echo Success.

codegen-compose:
	@dhall-to-yaml  <<< "(./docker-compose.dhall).dev" > ./docker-compose.yml.dev
	@dhall-to-yaml  <<< "(./docker-compose.dhall).img" > ./docker-compose.yml.img

compose-down:
	docker-compose down

compose-up:
	docker-compose up

compose-restart: compose-down codegen-compose compose-up

up-stage:
	oc -n monocle-stage apply -f deployment/
