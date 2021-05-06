# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

MESSAGES = monocle/config.proto monocle/search.proto monocle/task_data.proto
PINCLUDE = -I /usr/include $(PROTOC_FLAGS) -I ./protos/

codegen: codegen-python codegen-javascript codegen-stubs codegen-openapi codegen-haskell

codegen-stubs:
	mkdir -p srcgen/
	(cd codegen; cabal run monocle-codegen ../protos/monocle/http.proto ../haskell/src/Monocle/WebApi.hs ../monocle/webapi.py ../srcgen/WebApi.res)
	~/.cabal/bin/ormolu -i ./haskell/src/Monocle/WebApi.hs
	black ./monocle/webapi.py
	./web/node_modules/.bin/bsc -format ./srcgen/WebApi.res > ./web/src/components/WebApi.res
	rm -Rf srcgen/

codegen-haskell:
	sh -c 'for pb in $(MESSAGES); do ~/.cabal/bin/compile-proto-file --includeDir /usr/include --includeDir protos/ --proto $${pb} --out src/; done'
	find haskell/ -type f -name "*.hs" -exec ~/.cabal/bin/ormolu -i {} \;

codegen-python:
	protoc $(PINCLUDE) --python_out=./monocle/messages --mypy_out=./monocle/messages $(MESSAGES)
	mv monocle/messages/monocle/* monocle/messages/
	black monocle/messages/*.py*
	rm -Rf monocle/messages/monocle

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
