.PHONY: test
test:
	rustc --test -o test -g json.rs
	./test

.PHONY: parser
parser: test
	rustc -o parser parser.rs


.PHONY: parser-test
parser-test:
	./parser-test.sh
