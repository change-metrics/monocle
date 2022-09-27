# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later
.PHONY: up-stage


BASEDIR = monocle/protob
MESSAGES = $(BASEDIR)/search.proto $(BASEDIR)/config.proto $(BASEDIR)/login.proto $(BASEDIR)/metric.proto $(BASEDIR)/auth.proto
CRAWLER = $(BASEDIR)/change.proto $(BASEDIR)/crawler.proto
PINCLUDE = -I /usr/include $(PROTOC_FLAGS) -I ./schemas/

codegen: codegen-ci codegen-javascript codegen-stubs codegen-openapi codegen-haskell doc/architecture.png

codegen-ci:
	echo "(./.github/workflows/ci.dhall).Nix" | dhall-to-yaml > .github/workflows/nix.yaml
	echo "(./.github/workflows/ci.dhall).NixBuild" | dhall-to-yaml > .github/workflows/nix-build.yaml
	echo "(./.github/workflows/ci.dhall).Web" | dhall-to-yaml > .github/workflows/web.yaml
	echo "(./.github/workflows/ci.dhall).Docker" | dhall-to-yaml > .github/workflows/docker.yaml
	echo "(./.github/workflows/ci.dhall).Publish-Master-Image" | dhall-to-yaml > .github/workflows/publish-master.yaml
	echo "(./.github/workflows/ci.dhall).Publish-Tag-Image" | dhall-to-yaml > .github/workflows/publish-tag.yaml

doc/architecture.png: doc/architecture.plantuml
	plantuml ./doc/architecture.plantuml

codegen-stubs:
	mkdir -p srcgen/
	(cabal -fcodegen run monocle-codegen ./schemas/$(BASEDIR)/http.proto ./src/Monocle/Client/Api.hs ./src/Monocle/Servant/HTTP.hs ./srcgen/WebApi.res)
	fourmolu -i ./src/Monocle/Client/Api.hs ./src/Monocle/Servant/HTTP.hs
	./web/node_modules/.bin/bsc -format ./srcgen/WebApi.res > ./web/src/components/WebApi.res
	rm -Rf srcgen/

codegen-haskell:
	sh -c 'for pb in $(MESSAGES) $(CRAWLER); do compile-proto-file --includeDir /usr/include --includeDir schemas/ --includeDir ${PROTOBUF_SRC} --proto $${pb} --out codegen/; done'
	find codegen/Monocle -type f -name "*.hs" -exec sed -i {} -e '1i{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}' \;
	fourmolu -i codegen/Monocle

codegen-javascript:
	rm -f web/src/messages/*
	sh -c 'for pb in $(MESSAGES); do ocaml-protoc $(PINCLUDE) -bs -ml_out web/src/messages/ schemas/$${pb}; done'
	python3 ./codegen/rename_bs_module.py ./web/src/messages/

codegen-openapi:
	protoc $(PINCLUDE) --openapi_out=./doc/ $(BASEDIR)/http.proto
	@echo Created doc/openapi.yaml
