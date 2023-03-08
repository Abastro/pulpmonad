{-# LANGUAGE OverloadedStrings #-}

module Pulp.Desk.System.Hardware.MemoryStatus (memoryRatios, memoryUsed, memoryStat) where

import Control.Applicative
import Generic.Data
import Pulp.Desk.System.Hardware.Commons
import Pulp.Desk.Utils.ParseHor
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
memoryStat = parseFile memory ("/" </> "proc" </> "meminfo")
  where
    memory = do
      fields (symbolH ":" *> mayKB) >>= exQueryMap query

    mayKB = label "data" $ decimalH <* optional (symbolH "kB")
    query = do
      memTotal <- queryField "MemTotal"
      memFree <- queryField "MemFree"
      memAvailable <- queryField "MemAvailable"
      memBuffers <- queryField "Buffers"
      memCached <- queryField "Cached"
      swapTotal <- queryField "SwapTotal"
      swapFree <- queryField "SwapFree"
      pure MkMemoryStat{..}
