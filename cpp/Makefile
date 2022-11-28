CXXFLAGS=-std=c++2b -Wall -Wextra -pedantic -O2 -ggdb3
CXX=g++

.PHONY: test
test: json.o tests.o tests-main.o
	${CXX} -o tests json.o tests.o tests-main.o
	./tests

.PHONY: parser
parser: test parser.o json.o
	${CXX} -o parser parser.o json.o

.PHONY: parser-test
parser-test:
	./parser-test.sh
