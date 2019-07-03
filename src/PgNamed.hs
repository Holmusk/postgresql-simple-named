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
       ( NamedParam (..)
       , Name (..)

       , extractNames
       , namesToRow
       , (=?)
       ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty (..))
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
    { namedParamName  :: Name
    , namedParamParam :: PG.Action
    } deriving (Show)

-- | @PostgreSQL@ error type for named parameters.
data PgNamedError
    -- | Named parameter is not specified.
    = PgNamedParam Name
    -- | Query has no names inside but was called with named functions,
    | PgNoNames PG.Query
    -- | Query contains an empty name.
    | PgEmptyName PG.Query


-- | Type alias for 'PgNamedError'.
type WithError = MonadError PgNamedError

instance Show PgNamedError where
    show e = "PostgeSQL named parameter error: " ++ case e of
        PgNamedParam n -> "Named param '" ++ show n ++ "' is not specified"
        PgNoNames (PG.Query q) ->
            "Query has no names but was called with named functions: " ++ BS.unpack q
        PgEmptyName (PG.Query q) ->
            "Query contains empty name: " ++ BS.unpack q

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
NamedParam {namedParamName = "foo", namedParamParam = 1}

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
