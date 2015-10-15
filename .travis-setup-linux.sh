#!/bin/bash

set -xe
travis_retry sudo add-apt-repository -y ppa:hvr/ghc
travis_retry sudo apt-get update
travis_retry sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER # see note about happy/alex
travis_retry sudo apt-get install libxss-dev
cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
travis_retry cabal update
cabal install --only-dependencies --enable-tests --enable-benchmarks
