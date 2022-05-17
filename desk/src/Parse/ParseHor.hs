module Parse.ParseHor
  ( P.label,
    parseFile,
    skipH,
    remainH,
    identH,
    symbolH,
    decimalH,
    eoH
  )
where

import Control.Applicative
import Data.Char
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- | Parses a file using a parser.
parseFile :: P.Parsec Void T.Text a -> FilePath -> IO a
parseFile parser path =
  P.parse parser path <$> T.readFile path >>= \case
    Left err -> fail $ P.errorBundlePretty err
    Right result -> pure result

-- | Skips horizontally.
skipH :: P.MonadParsec e T.Text m => m ()
skipH = Lex.space P.hspace1 empty empty

-- | Parses remaining characters until a line break.
remainH :: P.MonadParsec e T.Text m => m T.Text
remainH = P.takeWhileP (Just "remaining") $ (/= '\n')

-- | Space-separated identifier, which could include '(' and ')'.
identH :: P.MonadParsec e T.Text m => m T.Text
identH = Lex.lexeme skipH (P.takeWhile1P (Just "identifier") isID)
  where
    isID c = isAlphaNum c || c == '(' || c == ')' || c == '_'

-- | Horizontal symbols.
symbolH :: P.MonadParsec e T.Text m => T.Text -> m T.Text
symbolH = Lex.symbol skipH

-- | Horizontal decimals.
decimalH :: (P.MonadParsec e T.Text m, Num a) => m a
decimalH = Lex.lexeme skipH Lex.decimal

-- | Parse an end of line, or eof.
eoH :: P.MonadParsec e T.Text m => m ()
eoH = () <$ P.eol <|> P.eof
