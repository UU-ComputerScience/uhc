#!/bin/bash
#
# Compiles the CoreApi demo, and runs it.

set -e

echo "Compiling Core generator..."
ghc -main-is CoreApi CoreApi.hs

echo "Running Core generator..."
./CoreApi

echo "Compile generated core to an executable using uhc..."
uhc CoreApiProg.bcr

echo "Running the generated executable..."
./CoreApiProg
