#!/bin/bash
set -e
mkdir codecov
cd codecov
cabal sandbox init
cabal install codecov-haskell
cd ..
mv dist/hpc/vanilla/* dist/hpc/
codecov/.cabal-sandbox/bin/codecov-haskell main --token=$CODECOV_TOKEN 2>&1 | grep -v $CODECOV_TOKEN
