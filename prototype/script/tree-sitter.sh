#!/bin/bash

cd `dirname $0`/../External/tree-sitter

case "$1" in
""|build)
	script/configure.sh
	make "$PRODUCT_NAME"
	;;
clean)
	script/clean.sh
	;;
*)
	echo "I don't know how to '$1'"
	exit 1
	;;
esac
