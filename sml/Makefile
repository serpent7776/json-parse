.PHONY: test
test:
	mlton -output test test.mlb
	./test

.PHONY: top
top:
	rlwrap poly --use json.sml

.PHONY: parser
parser:
	mlton -output ./parser parser.mlb
