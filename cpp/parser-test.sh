#!/usr/bin/env sh
time for f in ../json/*.json; do echo $f; time ./parser -n $f || exit 1; done
