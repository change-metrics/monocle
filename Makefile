# TODO: use a container
all: codegen-python codegen-javascript codegen-stubs

codegen-stubs:
	(cd codegen; cabal run monocle-codegen ../protos/monocle/http.proto ../monocle/webapi.py ../web/src/components/WebApi.res.raw)
	black ./monocle/webapi.py

codegen-python:
	protoc -I=./protos/ --python_out=./monocle/messages monocle/config.proto monocle/task_data.proto
	mv monocle/messages/monocle/* monocle/messages/
	black monocle/messages/*.py
	rm -Rf monocle/messages/monocle

codegen-javascript:
	mkdir -p srcgen/
	ocaml-protoc -bs -ml_out srcgen/ protos/monocle/config.proto
	./web/node_modules/.bin/bsc -format ./srcgen/config_bs.ml > web/src/messages/Config.res
	./web/node_modules/.bin/bsc -format ./srcgen/config_types.ml > web/src/messages/ConfigTypes.res
	sed -e 's/Config_types/ConfigTypes/g' -i web/src/messages/Config.res
	ocaml-protoc -bs -ml_out srcgen/ protos/monocle/task_data.proto
	./web/node_modules/.bin/bsc -format ./srcgen/task_data_bs.ml > web/src/messages/TaskData.res
	./web/node_modules/.bin/bsc -format ./srcgen/task_data_types.ml > web/src/messages/TaskDataTypes.res
	sed -e 's/Task_data_types/TaskDataTypes/g' -i web/src/messages/TaskData.res
	rm -Rf srcgen/
