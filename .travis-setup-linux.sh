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
rm -f ${HOME}/.cabal/config

travis_retry cabal update

echo 'repository head.hackage'                                                        >> ${HOME}/.cabal/config
echo '   url: http://head.hackage.haskell.org/'                                       >> ${HOME}/.cabal/config
echo '   secure: True'                                                                >> ${HOME}/.cabal/config
echo '   root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740' >> ${HOME}/.cabal/config
echo '              2e8555dde16ebd8df076f1a8ef13b8f14c66bad8eafefd7d9e37d0ed711821fb' >> ${HOME}/.cabal/config
echo '              8f79fd2389ab2967354407ec852cbe73f2e8635793ac446d09461ffb99527f6e' >> ${HOME}/.cabal/config
echo '   key-threshold: 3'                                                            >> ${HOME}/.cabal.config
sed -i 's/-- allow-newer: .*/allow-newer: *:base/' ${HOME}/.cabal/config

ecat ${HOME}/.cabal/config

travis_retry cabal update

cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls
