#!/usr/bin/env sh
time for f in ../json/*.json; do echo $f; ./parser -p $f | jq > /dev/null || exit 1; done
