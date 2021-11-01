# postgresql-simple-named

![Logo](https://user-images.githubusercontent.com/4276606/68105647-408b7c00-fef0-11e9-8d70-d3fbf314a647.png)

[![Build status](https://img.shields.io/travis/Holmusk/postgresql-simple-named.svg?logo=travis)](https://travis-ci.org/Holmusk/postgresql-simple-named)
[![Hackage](https://img.shields.io/hackage/v/postgresql-simple-named.svg?logo=haskell)](https://hackage.haskell.org/package/postgresql-simple-named)
[![Stackage Lts](http://stackage.org/package/postgresql-simple-named/badge/lts)](http://stackage.org/lts/package/postgresql-simple-named)
[![Stackage Nightly](http://stackage.org/package/postgresql-simple-named/badge/nightly)](http://stackage.org/nightly/package/postgresql-simple-named)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

This library introduces the implementation of named parameters for the
[`postgresql-simple`][pgs] library. `postgresql-simple-named` is considered to
be used along with the [`postgresql-simple`][pgs] library, so you could refer
there for the original documentation of primary functions. This package solves
exclusively one particular problem — gives the ability to use named parameters
instead of `?` in quasi-quoter queries and offers essential functions for substituting
variables in queries (`queryNamed`, `executeNamed`).

## Example

Operator `=?` binds named parameters with the corresponding values. Named
parameters inside SQL query start with the '?' character and can contain
lowercase and uppercase letters, digits and underscore. Below you can find a
basic example of how query with named parameters could look like:

```haskell
queryNamed dbConnection [sql|
    SELECT
        id, name, city
    FROM users
    WHERE name = ?nameParam
      AND age  = ?ageParam

|] [ "nameParam" =? "John"
   , "ageParam"  =? 42
   ]
```

This feature can be extremely helpful when the query uses some parameters more than once:

```haskell
query dbConnection [sql|
    SELECT
        col1, col2
    FROM my_table
    WHERE id = ?
      AND (? IS NULL OR id > ? )
      AND (? IS NULL OR id < ? )

|] (someId, minId, minId, maxId, maxId)
```

This is how the query looks like with the `postgresql-simple` library. You can
rewrite it the following way using the `postgresql-simple-named` library:

```haskell
queryNamed dbConnection [sql|
    SELECT
        col1, col2
    FROM my_table
    WHERE id = ?someId
      AND (?minId IS NULL OR id > ?minId )
      AND (?maxId IS NULL OR id < ?maxId )

|] [ "someId" =? 42
   , "minId"  =? 1
   , "maxId"  =? 100
   ]
```

## How to build

Build the library with either `cabal new-build` or `stack build`.

## How to test locally

* Run DB in a Docker in a separate terminal window using command:
  ```bash
  docker run -p 5432:5432 \
      -e POSTGRES_USER=postgres \
      -e POSTGRES_PASSWORD=helloci \
      -e POSTGRES_DB=pg_named \
      postgres:12
  ```
* Run tests using `cabal new-test` or `stack test`


[pgs]: https://hackage.haskell.org/package/postgresql-simple
