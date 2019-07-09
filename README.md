# postgresql-simple-named

[![Hackage](https://img.shields.io/hackage/v/postgresql-simple-named.svg?logo=haskell)](https://hackage.haskell.org/package/postgresql-simple-named)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/postgresql-simple-named/badge/lts)](http://stackage.org/lts/package/postgresql-simple-named)
[![Stackage Nightly](http://stackage.org/package/postgresql-simple-named/badge/nightly)](http://stackage.org/nightly/package/postgresql-simple-named)
[![Build status](https://img.shields.io/travis/Holmusk/postgresql-simple-named.svg?logo=travis)](https://travis-ci.org/Holmusk/postgresql-simple-named)

Implementation of named parameters for `postgresql-simple` library

## How to build

Build the library with either `cabal new-build` or `stack build`.

## How to test locally

* Run DB in a Docker in a separate terminal window using command:
  ```
  docker run -p 5432\:5432 -e POSTGRES_USER=root -e POSTGRES_DB=pg-named postgres\:10.5-alpine
  ```
* Run tests using `cabal new-test` or `stack test`
