# TODO: use a container
all: codegen-python codegen-javascript

codegen-python:
	protoc -I=./protos/ --python_out=./monocle/messages monocle/config.proto
	mv monocle/messages/monocle/* monocle/messages/
	black monocle/messages/*.py
	rm -Rf monocle/messages/monocle

codegen-javascript:
	mkdir -p srcgen/
	ocaml-protoc -bs -ml_out srcgen/ protos/monocle/config.proto
	./web/node_modules/.bin/bsc -format ./srcgen/config_bs.ml > web/src/messages/Config.res
	./web/node_modules/.bin/bsc -format ./srcgen/config_types.ml > web/src/messages/ConfigTypes.res
	sed -e 's/Config_types/ConfigTypes/g' -i web/src/messages/Config.res
	rm -Rf srcgen/
