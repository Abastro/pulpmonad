module Pulp.Desk.PulpPath (
  module System.FilePath,
  initDataDir,
  dataPath,
) where

import Data.IORef
import System.FilePath
import System.IO.Unsafe

{-# NOINLINE dataDir #-}
dataDir :: IORef FilePath
dataDir = unsafePerformIO $ newIORef (error "not initialized")

-- | Data directory need to be initialized exactly once, in the beginning.
initDataDir :: FilePath -> IO ()
initDataDir = writeIORef dataDir

-- | Child path from data directory.
dataPath :: FilePath -> IO FilePath
dataPath subPath = (</> subPath) <$> readIORef dataDir
