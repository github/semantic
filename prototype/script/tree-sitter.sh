#!/bin/bash

cd `dirname $0`/External/tree-sitter
script/configure.sh
make compiler
make runtime
