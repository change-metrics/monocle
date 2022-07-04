# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later
.PHONY: up-stage


BASEDIR = monocle/protob
MESSAGES = $(BASEDIR)/search.proto $(BASEDIR)/config.proto $(BASEDIR)/login.proto $(BASEDIR)/metric.proto $(BASEDIR)/auth.proto
CRAWLER = $(BASEDIR)/change.proto $(BASEDIR)/crawler.proto
PINCLUDE = -I /usr/include $(PROTOC_FLAGS) -I ./protos/

codegen: codegen-ci codegen-javascript codegen-stubs codegen-openapi codegen-haskell doc/architecture.png

codegen-ci: .github/workflows/nix.yaml
.github/workflows/nix.yaml: .github/workflows/ci.dhall .github/workflows/mkCI.dhall
	echo "(./.github/workflows/ci.dhall).Nix" | dhall-to-yaml > .github/workflows/nix.yaml

doc/architecture.png: doc/architecture.plantuml
	plantuml ./doc/architecture.plantuml

codegen-stubs:
	mkdir -p srcgen/
	(cd codegen; cabal run monocle-codegen ../protos/$(BASEDIR)/http.proto ../haskell/src/Monocle/Client/Api.hs ../haskell/src/Monocle/Servant/HTTP.hs ../srcgen/WebApi.res)
	ormolu -i ./haskell/src/Monocle/Client/Api.hs ./haskell/src/Monocle/Servant/HTTP.hs
	./web/node_modules/.bin/bsc -format ./srcgen/WebApi.res > ./web/src/components/WebApi.res
	rm -Rf srcgen/

codegen-haskell:
	sh -c 'for pb in $(MESSAGES) $(CRAWLER); do compile-proto-file --includeDir /usr/include --includeDir protos/ --includeDir ${PROTOBUF_SRC} --proto $${pb} --out haskell/codegen/; done'
	find haskell/codegen/ -type f -name "*.hs" -exec sed -i {} -e '1i{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}' \;
	find haskell/codegen/ -type f -name "*.hs" -exec ormolu -i {} \;

codegen-javascript:
	rm -f web/src/messages/*
	sh -c 'for pb in $(MESSAGES); do ocaml-protoc $(PINCLUDE) -bs -ml_out web/src/messages/ protos/$${pb}; done'
	python3 ./codegen/rename_bs_module.py ./web/src/messages/

codegen-openapi:
	protoc $(PINCLUDE) --openapi_out=./doc/ $(BASEDIR)/http.proto
	@echo Created doc/openapi.yaml

up-stage:
	oc -n monocle-stage apply -f deployment/
