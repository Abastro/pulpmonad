module Pulp.Desk.System.Hardware.Commons (
  readSingleLine,
  ratioTo,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO

-- | Reads single line from a file.
readSingleLine :: FilePath -> IO T.Text
readSingleLine path = withFile path ReadMode T.hGetLine

-- | Ratio to certain value
ratioTo :: (Applicative v, Real a, Fractional b) => a -> v a -> v b
ratioTo un = fmap (\n -> realToFrac n / realToFrac un)
