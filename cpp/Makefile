CPPFLAGS=-std=c++2b -Wall -Wextra -pedantic -O2 -ggdb3
CXX=g++

.PHONY: test
test: json.o tests.o tests-main.o
	${CXX} -o tests json.o tests.o tests-main.o
	./tests

.PHONY: parser
parser: test
	${CXX} -o parser ${CFLAGS} parser.cpp

.PHONY: parser-test
parser-test:
	./parser-test.sh
