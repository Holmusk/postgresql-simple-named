{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

{- | Introduces named parameters for @postgresql-simple@ library.
It uses @?@ question mark symbol as the indicator of the named parameter which
is replaced with the standard syntax with question marks. Check out the example
of usage:

@
queryNamed [sql|
    SELECT *
    FROM users
    WHERE foo = ?foo
      AND bar = ?bar
      AND baz = ?foo
|] [ "foo" =? "fooBar"
   , "bar" =? "barVar"
   ]
@
-}

module PgNamed
       ( -- * Named data types and smart constructors
         NamedParam (..)
       , Name (..)
       , (=?)

         -- * Errors
       , PgNamedError (..)

         -- * Functions to deal with named parameters
       , extractNames
       , namesToRow

         -- * Database querying functions with named parameters
       , queryNamed
       , executeNamed
       ) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Exts (IsString)

import qualified Data.ByteString.Char8 as BS
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.Types as PG


-- | Wrapper over name of the argument.
newtype Name = Name
    { unName :: Text
    } deriving newtype (Show, Eq, Ord, IsString)

-- | Data type to represent each named parameter.
data NamedParam = NamedParam
    { namedParamName  :: !Name
    , namedParamParam :: !PG.Action
    } deriving (Show)

-- | @PostgreSQL@ error type for named parameters.
data PgNamedError
    -- | Named parameter is not specified.
    = PgNamedParam Name
    -- | Query has no names inside but was called with named functions,
    | PgNoNames PG.Query
    -- | Query contains an empty name.
    | PgEmptyName PG.Query
  deriving (Eq)


-- | Type alias for monads that can throw errors of the 'PgNamedError' type.
type WithError = MonadError PgNamedError

instance Show PgNamedError where
    show e = "PostgreSQL named parameter error: " ++ case e of
        PgNamedParam n -> "Named parameter '" ++ show n ++ "' is not specified"
        PgNoNames (PG.Query q) ->
            "Query has no names but was called with named functions: " ++ BS.unpack q
        PgEmptyName (PG.Query q) ->
            "Query contains an empty name: " ++ BS.unpack q

-- | Checks whether the 'Name' is in the list and returns its parameter.
lookupName :: Name -> [NamedParam] -> Maybe PG.Action
lookupName n = lookup n . map (\NamedParam{..} -> (namedParamName, namedParamParam))

{- | This function takes query with named parameters specified like this:

@
SELECT name, user FROM users WHERE id = ?id
@

and returns either the error or query with all all names replaced by
questiosn marks @?@ with list of the names in the order of their appearance.

For example:

>>> extractNames "SELECT * FROM users WHERE foo = ?foo AND bar = ?bar AND baz = ?foo"
Right ("SELECT * FROM users WHERE foo = ? AND bar = ? AND baz = ?","foo" :| ["bar","foo"])
-}
extractNames
    :: PG.Query
    -> Either PgNamedError (PG.Query, NonEmpty Name)
extractNames qr = go (PG.fromQuery qr) >>= \case
    (_, [])         -> Left $ PgNoNames qr
    (q, name:names) -> Right (PG.Query q, name :| names)
  where
    go :: ByteString -> Either PgNamedError (ByteString, [Name])
    go str
        | BS.null str = Right ("", [])
        | otherwise   = let (before, after) = BS.break (== '?') str in
            case BS.uncons after of
                Nothing -> Right (before, [])
                Just ('?', nameStart) ->
                    let (name, remainingQuery) = BS.span isNameChar nameStart
                    in if BS.null name
                           then Left $ PgEmptyName qr
                           else fmap (bimap ((before <> "?") <>) (Name (decodeUtf8 name) :))
                                     (go remainingQuery)
                Just _ -> error "'break (== '?')' doesn't return string started with the question mark"

    isNameChar :: Char -> Bool
    isNameChar c = isAlphaNum c || c == '_'


-- | Returns the list of values to use in query by given list of 'Name's.
namesToRow
    :: forall m . WithError m
    => NonEmpty Name  -- ^ List of the names used in query
    -> [NamedParam]   -- ^ List of the named parameters
    -> m (NonEmpty PG.Action)
namesToRow names params = traverse magicLookup names
  where
    magicLookup :: Name -> m PG.Action
    magicLookup n = case lookupName n params of
        Just x  -> pure x
        Nothing -> throwError $ PgNamedParam n

{- | Operator to create 'NamedParam's.

>>> "foo" =? (1 :: Int)
NamedParam {namedParamName = "foo", namedParamParam = Plain "1"}

So it can be used in creating the list of the named arguments:

@
queryNamed [sql|
  SELECT * FROM users WHERE foo = ?foo AND bar = ?bar AND baz = ?foo"
|] [ "foo" =? "fooBar"
   , "bar" =? "barVar"
   ]
@
-}
infix 7 =?
(=?) :: (PG.ToField a) => Name -> a -> NamedParam
n =? a = NamedParam n $ PG.toField a
{-# INLINE (=?) #-}

{- | Queries the database with a given query and named parameters
and expects a list of rows in return.

@
queryNamed dbConnection [sql|
    SELECT id FROM table
    WHERE foo = ?foo
|] [ "foo" '=?' "bar" ]
@
-}
queryNamed
    :: (MonadIO m, WithError m, PG.FromRow res)
    => PG.Connection  -- ^ Database connection
    -> PG.Query       -- ^ Query with named parameters inside
    -> [NamedParam]   -- ^ The list of named parameters to be used in the query
    -> m [res]        -- ^ Resulting rows
queryNamed conn qNamed params =
    withNamedArgs qNamed params >>= \(q, actions) ->
        liftIO $ PG.query conn q (toList actions)

{- | Modifies the database with a given query and named parameters
and expects a number of the rows affected.

@
executeNamed dbConnection [sql|
    UPDATE table
    SET foo = 'bar'
    WHERE id = ?id
|] [ "id" '=?' someId ]
@
-}
executeNamed
    :: (MonadIO m, WithError m)
    => PG.Connection  -- ^ Database connection
    -> PG.Query       -- ^ Query with named parameters inside
    -> [NamedParam]   -- ^ The list of named parameters to be used in the query
    -> m Int64        -- ^ Number of the rows affected by the given query
executeNamed conn qNamed params =
    withNamedArgs qNamed params >>= \(q, actions) ->
        liftIO $ PG.execute conn q (toList actions)

-- | Helper to use named parameters.
withNamedArgs
    :: WithError m
    => PG.Query
    -> [NamedParam]
    -> m (PG.Query, NonEmpty PG.Action)
withNamedArgs qNamed namedArgs = do
    (q, names) <- case extractNames qNamed of
        Left errType -> throwError errType
        Right r      -> pure r
    args <- namesToRow names namedArgs
    pure (q, args)
