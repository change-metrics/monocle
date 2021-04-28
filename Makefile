# TODO: use a container
all: codegen-python

codegen-python:
	protoc -I=./protos/ --python_out=./monocle/messages monocle/config.proto
	mv monocle/messages/monocle/* monocle/messages/
	black monocle/messages/*.py
	rm -Rf monocle/messages/monocle
