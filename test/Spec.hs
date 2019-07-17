{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (Spec, describe, hspec, it, shouldReturn)

import PgNamed (NamedParam, PgNamedError (..), queryNamed, queryWithNamed, (=?))

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromRow as Sql


connectionSettings :: ByteString
connectionSettings = "host=localhost port=5432 user=postgres dbname=pg_named"

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    dbPool <- Pool.createPool (Sql.connectPostgreSQL connectionSettings) Sql.close 10 5 10
    hspec $ unitTests dbPool

unitTests :: Pool.Pool Sql.Connection -> Spec
unitTests dbPool = describe "Testing: postgresql-simple-named" $ do
    it "returns error when named parameter is not specified" $
        missingNamedParam `shouldReturn` Left (PgNamedParam "bar")
    it "no named parameters in a query" $
        noNamedParams `shouldReturn` Left (PgNoNames "SELECT 42")
    it "empty name in a query with named parameters" $
        emptyName `shouldReturn` Left (PgEmptyName "SELECT ?foo, ?")
    it "named parameters are parsed and passed correctly" $
        queryTestValue `shouldReturn` Right (TestValue 42 42 "baz")
    it "named parameters are parsed correctly by user defined row parser" $
        queryWithTestValue `shouldReturn` Right (TestValue 42 42 "baz")
  where
    missingNamedParam :: IO (Either PgNamedError TestValue)
    missingNamedParam = run "SELECT ?foo, ?bar" ["foo" =? True]

    noNamedParams :: IO (Either PgNamedError TestValue)
    noNamedParams = run "SELECT 42" []

    emptyName :: IO (Either PgNamedError TestValue)
    emptyName = run "SELECT ?foo, ?" ["foo" =? True]

    queryTestValue :: IO (Either PgNamedError TestValue)
    queryTestValue = run "SELECT ?intVal, ?intVal, ?txtVal"
        [ "intVal" =? (42 :: Int)
        , "txtVal" =? ("baz" :: ByteString)
        ]

    queryWithTestValue :: IO (Either PgNamedError TestValue)
    queryWithTestValue = runQueryWith testValueParser "SELECT ?intVal, ?intVal, ?txtVal"
        [ "intVal" =? (42 :: Int)
        , "txtVal" =? ("baz" :: ByteString)
        ]

    run :: Sql.Query -> [NamedParam] -> IO (Either PgNamedError TestValue)
    run q params = runNamedQuery $ Pool.withResource dbPool (\conn -> queryNamed conn q params)

    runQueryWith
        :: Sql.RowParser TestValue
        -> Sql.Query
        -> [NamedParam]
        -> IO (Either PgNamedError TestValue)
    runQueryWith rowParser q params = runNamedQuery $
        Pool.withResource dbPool (\conn -> queryWithNamed rowParser conn q params)

runNamedQuery :: ExceptT PgNamedError IO [TestValue] -> IO (Either PgNamedError TestValue)
runNamedQuery = fmap (second head) . runExceptT

data TestValue = TestValue
    { intVal1 :: !Int
    , intVal2 :: !Int
    , txtVal  :: !ByteString
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (Sql.FromRow, Sql.ToRow)

testValueParser :: Sql.RowParser TestValue
testValueParser = do
    intVal1 <- Sql.field
    intVal2 <- Sql.field
    txtVal  <- Sql.field
    return TestValue{..}
