#!/bin/bash

set -xe

# https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/templates/header.sh
travis_retry() {
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    # ! { } ignores set -e, see https://stackoverflow.com/a/4073372
    ! { "$@"; result=$?; }
    [ $result -eq 0 ] && break
    count=$(($count + 1))
    sleep 1
  done

  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }

  return $result
}

sudo add-apt-repository -y ppa:hvr/ghc
travis_retry sudo apt-get update
travis_retry sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER # see note about happy/alex
travis_retry sudo apt-get install libxss-dev libx11-dev libxrandr-dev libxinerama-dev
cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
travis_retry cabal update
cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls
