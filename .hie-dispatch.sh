#!/bin/bash

if [ -x "$(command -v bazel)" ]; then
    source ".hie-bios.sh"
else
    source "script/ghci-flags"
fi
