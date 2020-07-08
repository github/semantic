#!/usr/bin/env bash

# This file is invoked by ghcide to load the packages
# required to load the semantic monorepo into ghci.
# You generally shouldn't need to modify it.

set -euo pipefail
bazel build //:hie-bios --output_groups=hie_bios
cat bazel-bin/hie-bios@hie-bios >"$HIE_BIOS_OUTPUT"
# Make warnings non-fatal
echo -Wwarn >>"$HIE_BIOS_OUTPUT"
