-- | Module for horizontal parsing.
module Pulp.Desk.Utils.ParseHor (
  ParseHor (parsec),
  labelH,
  parseFile,
  skipH,
  remainH,
  identH,
  identCondH,
  symbolH,
  decimalH,
  signedDecimalH,
  eoH,
  fieldsCustom,
  fields,
  fieldsIgnoring,
  exQueryMap,
  queryOpt,
  queryOptAs,
  queryField,
  queryFieldAs,
  queryAll,
  queryAllAs,
) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Printf

-- Ideally, these need to be tested. Such test is not so useful, though.

-- | Monad for horizontal parsing.
newtype ParseHor a = AsParseHor {parsec :: Parse.Parsec Void T.Text a}
  deriving (Functor, Applicative, Monad, Alternative, MonadFail)

-- | Parses a file using a parser. Throws userError when fails.
parseFile :: ParseHor a -> FilePath -> IO a
parseFile parser path = do
  txt <- T.readFile path
  case Parse.parse parser.parsec path txt of
    Left err -> fail $ Parse.errorBundlePretty err
    Right result -> pure result

-- | Skips horizontal spaces.
skipH :: ParseHor ()
skipH = AsParseHor Parse.hspace

-- | Parses remaining characters until a line break.
-- The line break itself is not consumed.
remainH :: ParseHor T.Text
remainH = AsParseHor $ Parse.takeWhileP (Just "remaining") (/= '\n')

-- | Space-separated identifier, which could include numbers, '_', '(' and ')'.
identH :: ParseHor T.Text
identH = AsParseHor $ Lex.lexeme skipH.parsec (Parse.takeWhile1P (Just "identifier") isID)
  where
    isID c = isAlphaNum c || c == '(' || c == ')' || c == '_'

-- | Identifier with restricting condition.
identCondH :: (T.Text -> Bool) -> ParseHor T.Text
identCondH cond = AsParseHor $ Parse.try $ do
  ident <- identH.parsec
  ident <$ guard (cond ident)

-- | Horizontal symbols.
symbolH :: T.Text -> ParseHor T.Text
symbolH = AsParseHor . Lex.symbol skipH.parsec

-- | Horizontal decimals.
decimalH :: Num a => ParseHor a
decimalH = AsParseHor $ Lex.lexeme skipH.parsec Lex.decimal

-- | Horizontal signed decimals.
-- Since space is a separator, we do not allow 
signedDecimalH :: Num a => ParseHor a
signedDecimalH = AsParseHor $ Lex.lexeme skipH.parsec $ Lex.signed (pure ()) Lex.decimal

-- | Parse an end of line, or eof.
eoH :: ParseHor ()
eoH = AsParseHor $ void Parse.eol <|> Parse.eof

-- | Names expected token with the provided label when it fails without consuming input.
labelH :: String -> ParseHor a -> ParseHor a
labelH name parse = AsParseHor $ Parse.label name parse.parsec

-- | Parse fields with custom parser for reading the field name.
fieldsCustom :: ParseHor T.Text -> ParseHor a -> ParseHor (M.Map T.Text a)
fieldsCustom custom pval = M.fromList <$> field `sepEndBy` eoH
  where
    field = labelH "field" $ (,) <$> custom <*> pval

-- | Parse fields as a map.
fields :: ParseHor a -> ParseHor (M.Map T.Text a)
fields = fieldsCustom identH

-- | Parse fields as a map, ignoring provided heading for each line.
fieldsIgnoring :: ParseHor hd -> ParseHor a -> ParseHor (M.Map T.Text a)
fieldsIgnoring hd = fieldsCustom (hd *> identH)

-- | Map query monad.
newtype QueryMap v a = QueryMap (ExceptT String (State (M.Map T.Text v)) a)
  deriving (Functor, Applicative, Monad)

instance MonadFail (QueryMap v) where
  fail :: String -> QueryMap v a
  fail = QueryMap . throwError

-- | Execute QueryMap.
exQueryMap :: (MonadFail m, Show v) => QueryMap v a -> M.Map T.Text v -> m a
exQueryMap (QueryMap query) st = either (fail . errMsg) pure $ evalState (runExceptT query) st
  where
    errMsg msg = printf "Error: %s\nIll-formed map: %s\n" msg (show st)

-- | Query an optional field.
queryOpt :: T.Text -> QueryMap v (Maybe v)
queryOpt name = QueryMap . state $ M.updateLookupWithKey (\_ _ -> Nothing) name

-- | Query an optional field with a mapping function.
queryOptAs :: T.Text -> (v -> Maybe r) -> QueryMap v (Maybe r)
queryOptAs name f = (>>= f) <$> queryOpt name

-- | Query a field.
queryField :: T.Text -> QueryMap v v
queryField name = queryOpt name >>= maybe (fail notFoundErr) pure
  where
    notFoundErr = printf "Field %s not found" name

-- | Query a field with a mapping function.
queryFieldAs :: T.Text -> (v -> Maybe r) -> QueryMap v r
queryFieldAs name f = queryField name >>= maybe (fail illFormedErr) pure . f
  where
    illFormedErr = printf "Field %s ill-formed" name

-- | Query all fields satisfying predicate.
queryAll :: (T.Text -> Bool) -> QueryMap b (M.Map T.Text b)
queryAll pred = QueryMap . state $ M.partitionWithKey (\k _ -> pred k)

-- | Query all fields with predicate with a mapping function.
queryAllAs :: (T.Text -> Bool) -> (M.Map T.Text b -> Maybe r) -> QueryMap b r
queryAllAs pred f = do
  fs <- queryAll pred
  maybe (fail . illFormed $ M.keys fs) pure (f fs)
  where
    illFormed names = printf "Fields %s ill-formed" (show names)
