.PHONY: test
test:
	mlton -default-type int64 -output test test.mlb
	./test

.PHONY: top
top:
	rlwrap poly --use json.sml

.PHONY: parser
parser:
	mlton -default-type int64 -output ./parser parser.mlb
