export SHELL=/bin/bash

.PHONY: default

default:
	raco make lib.rkt && \
	raco scribble +m --dest doc manual.scrbl && \
	raco test lib.rkt

clean:
	git clean -fdX
