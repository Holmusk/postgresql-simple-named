{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE CPP                #-}

module Main (main) where

import Control.Exception (bracket_)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import GHC.Generics (Generic)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldReturn)

import PgNamed (NamedParam, PgNamedError (..), executeManyNamed, queryNamed, queryWithNamed, (=?))

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromRow as Sql


connectionSettings :: ByteString
connectionSettings = "host=localhost port=5432 user=postgres password=postgres dbname=postgres"

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    dbPool <- 
#if MIN_VERSION_resource_pool(0,4,0)
        Pool.newPool (Pool.defaultPoolConfig ((Sql.connectPostgreSQL connectionSettings)) Sql.close 1 10)
#else
        Pool.createPool (Sql.connectPostgreSQL connectionSettings) Sql.close 10 5 10
#endif
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
    it "executeManyNamed inserts multiple rows and returns the number of affected rows" $
        withRollback dbPool $ \conn -> do
            setupExecuteManyTable conn
            result <- insertRows conn [InsertRow 1 "foo", InsertRow 2 "bar", InsertRow 3 "baz"]
            result `shouldBe` Right (3 :: Int64)
            rows <- Sql.query_ conn "SELECT id, name FROM execute_many_test ORDER BY id"
                :: IO [(Int, ByteString)]
            rows `shouldBe` [(1, "foo"), (2, "bar"), (3, "baz")]
    it "executeManyNamed inserts five rows and selects them all back" $
        withRollback dbPool $ \conn -> do
            setupExecuteManyTable conn
            let fiveRows =
                    [ InsertRow 1 "aaa", InsertRow 2 "bbb", InsertRow 3 "ccc"
                    , InsertRow 4 "ddd", InsertRow 5 "eee"
                    ]
            result <- insertRows conn fiveRows
            result `shouldBe` Right (5 :: Int64)
            rows <- Sql.query_ conn "SELECT * FROM execute_many_test ORDER BY id"
                :: IO [(Int, ByteString)]
            rows `shouldBe` [(1, "aaa"), (2, "bbb"), (3, "ccc"), (4, "ddd"), (5, "eee")]
    it "executeManyNamed returns 0 when given an empty collection" $
        withRollback dbPool $ \conn -> do
            setupExecuteManyTable conn
            result <- insertRows conn []
            result `shouldBe` Right (0 :: Int64)
    it "executeManyNamed returns error when a named parameter is missing" $
        withRollback dbPool $ \conn -> do
            setupExecuteManyTable conn
            result <- runExceptT $ executeManyNamed conn
                "INSERT INTO execute_many_test (id, name) VALUES (?id, ?name)"
                (\InsertRow{..} -> ["id" =? rowId])
                [InsertRow 1 "foo"]
            result `shouldBe` Left (PgNamedParam "name")
    it "executeManyNamed returns error when query has no named parameters" $
        withRollback dbPool $ \conn -> do
            result <- runExceptT $
                executeManyNamed conn "SELECT 42" insertRowParams [InsertRow 1 "foo"]
            result `shouldBe` Left (PgNoNames "SELECT 42")
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
    queryWithTestValue = runWith testValueParser "SELECT ?intVal, ?intVal, ?txtVal"
        [ "intVal" =? (42 :: Int)
        , "txtVal" =? ("baz" :: ByteString)
        ]

    run :: Sql.Query -> [NamedParam] -> IO (Either PgNamedError TestValue)
    run = callQuery queryNamed

    runWith
        :: Sql.RowParser TestValue
        -> Sql.Query
        -> [NamedParam]
        -> IO (Either PgNamedError TestValue)
    runWith rowParser = callQuery (queryWithNamed rowParser)

    callQuery
        :: (Sql.Connection -> Sql.Query -> [NamedParam] -> ExceptT PgNamedError IO [TestValue])
        -> Sql.Query
        -> [NamedParam]
        -> IO (Either PgNamedError TestValue)
    callQuery f q params = Pool.withResource dbPool (\conn -> runNamedQuery $ f conn q params)

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

data InsertRow = InsertRow
    { rowId   :: !Int
    , rowName :: !ByteString
    }

insertRowParams :: InsertRow -> [NamedParam]
insertRowParams InsertRow{..} = ["id" =? rowId, "name" =? rowName]

-- | Runs the action inside a transaction that is always rolled back
-- afterwards, so any tables or rows it creates never outlive the test.
withRollback :: Pool.Pool Sql.Connection -> (Sql.Connection -> IO a) -> IO a
withRollback dbPool action = Pool.withResource dbPool $ \conn ->
    bracket_ (Sql.begin conn) (Sql.rollback conn) (action conn)

setupExecuteManyTable :: Sql.Connection -> IO ()
setupExecuteManyTable conn = do
    _ <- Sql.execute_ conn
        "CREATE TABLE execute_many_test (id INT NOT NULL, name TEXT NOT NULL)"
    pure ()

insertRows :: Sql.Connection -> [InsertRow] -> IO (Either PgNamedError Int64)
insertRows conn rows = runExceptT $ executeManyNamed conn
    "INSERT INTO execute_many_test (id, name) VALUES (?id, ?name)"
    insertRowParams
    rows
