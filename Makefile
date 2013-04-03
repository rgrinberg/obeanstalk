
build:
	obuild build
	otags -I ~/.opam/4.00.1/lib/type_conv/ -I ~/.opam/4.00.1/lib/sexplib -pa pa_type_conv.cma -pa pa_sexp_conv.cma . -r -vi

configure:
	obuild configure --enable-tests

all:
	make build
	make test

test:
	obuild test --output
	

