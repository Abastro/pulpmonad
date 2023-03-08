{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.MemoryStatus (memoryRatios, memoryUsed, memoryStat) where

import Control.Applicative
import Generic.Data
import Pulp.Desk.System.Hardware.Commons
import Pulp.Desk.Utils.ParseHor qualified as Parse
import System.FilePath

-- | Memory statistics. Usual unit is kB.
data MemoryStat a = MkMemoryStat
  { memTotal :: !a
  , memFree :: !a
  , memAvailable :: !a
  , memBuffers :: !a
  , memCached :: !a
  , swapTotal :: !a
  , swapFree :: !a
  }
  deriving stock (Show, Generic1)
  deriving (Functor, Applicative) via (Generically1 MemoryStat)

memoryRatios :: (Real a, Fractional b) => MemoryStat a -> MemoryStat b
memoryRatios mem = ratioTo mem.memTotal mem

memoryUsed :: Num a => MemoryStat a -> a
memoryUsed MkMemoryStat{..} = memTotal - memFree - memBuffers - memCached

-- | Gets Memory statistics.
-- Pulls from </proc/meminfo>.
memoryStat :: IO (MemoryStat Int)
memoryStat = Parse.parseFile memory ("/" </> "proc" </> "meminfo")
  where
    memory = do
      Parse.fields (Parse.symbolH ":" *> mayKB) >>= Parse.exQueryMap query

    mayKB = Parse.labelH "data" $ Parse.decimalH <* optional (Parse.symbolH "kB")
    query = do
      memTotal <- Parse.queryField "MemTotal"
      memFree <- Parse.queryField "MemFree"
      memAvailable <- Parse.queryField "MemAvailable"
      memBuffers <- Parse.queryField "Buffers"
      memCached <- Parse.queryField "Cached"
      swapTotal <- Parse.queryField "SwapTotal"
      swapFree <- Parse.queryField "SwapFree"
      pure MkMemoryStat{..}
