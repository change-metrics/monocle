# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

MESSAGES = monocle/config.proto monocle/search.proto monocle/task_data.proto
BACKEND_ONLY = monocle/change.proto monocle/crawler.proto
PINCLUDE = -I /usr/include $(PROTOC_FLAGS) -I ./protos/

codegen: codegen-python codegen-javascript codegen-stubs codegen-openapi codegen-haskell

codegen-stubs:
	mkdir -p srcgen/
	(cd codegen; cabal run monocle-codegen ../protos/monocle/http.proto ../haskell/src/Monocle/Api/Client/Api.hs ../haskell/src/Monocle/Servant/HTTP.hs ../monocle/webapi.py ../srcgen/WebApi.res)
	ormolu -i ./haskell/src/Monocle/Api/Client/Api.hs ./haskell/src/Monocle/Servant/HTTP.hs
	black ./monocle/webapi.py
	./web/node_modules/.bin/bsc -format ./srcgen/WebApi.res > ./web/src/components/WebApi.res
	rm -Rf srcgen/

codegen-haskell:
	sh -c 'for pb in $(MESSAGES) $(BACKEND_ONLY); do compile-proto-file --includeDir /usr/include --includeDir protos/ --includeDir ${PROTOBUF_SRC} --proto $${pb} --out haskell/codegen/; done'
	find haskell/codegen/ -type f -name "*.hs" -exec ormolu -i {} \;

codegen-python:
	protoc $(PINCLUDE) --python_out=./ --mypy_out=./ $(MESSAGES)
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
