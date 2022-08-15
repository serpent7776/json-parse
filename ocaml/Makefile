.PHONY: test
test:
	ocamlc -o test json.ml test.ml
	env CAMLRUNPARAM=b ./test

.PHONY: parser
parser: test
	ocamlopt -w A-4-50 -o parser json.ml parser.ml

.PHONY: top
top:
	ocamlmktop -o jsontop json.ml
	rlwrap ./jsontop

.PHONY: utop
utop:
	utop utop.ml
