{-# LANGUAGE LambdaCase #-}
module StartHook where

import Data.Coerce
import Data.Foldable
import qualified Data.Map.Strict as M
import Defines
import System.Environment
import XMonad
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import System.IO

modifyingM :: (ExtensionClass a) => (a -> X a) -> X ()
modifyingM f = XS.get >>= f >>= XS.put

-- TODO Is restart required at all?
newtype PerformRestartable s = PerformRestartable (M.Map String s)
  deriving (Read, Show)

type TypRS s = (Typeable s, Read s, Show s)

instance (TypRS s) => ExtensionClass (PerformRestartable s) where
  initialValue = PerformRestartable mempty
  extensionType = PersistentExtension

-- | Performs a restart-aware action. ID should be uniquely given.
performRestartable :: forall s. (TypRS s) => String -> X s -> (s -> X s) -> X ()
performRestartable ident initial restart =
  modifyingM @(PerformRestartable s) $ fmap coerce . M.alterF withState ident . coerce
  where
    withState = fmap Just . maybe initial restart

-- | Perform an action only once, alike `spawnOnce`. ID should be uniquely given.
performOnce :: String -> X () -> X ()
performOnce ident run = performRestartable ident run (const $ pure ())

-- | Copies configurations.
copyConfig :: FilePath -> X ()
copyConfig mainDir = do
  liftIO $ do
    gtkrc <- getEnv "GTK2_RC_FILES"
    copyFile (mainDir </> "cfg" </> ".gtkrc-2.0") gtkrc
  liftIO $ do
    gtk3 <- getXdgDirectory XdgConfig "gtk-3.0"
    copyFile (mainDir </> "cfg" </> "settings.ini") (gtk3 </> "settings.ini")

-- | Initiate programs.
initiatePrograms :: X ()
initiatePrograms = performOnce "initiate" $ do
  liftIO $ hPrintf stderr "[Normal] Initiating programs... \n"
  safeSpawnProg "status-notifier-watcher"
  safeSpawnProg "pasystray"
  safeSpawn "nm-applet" ["--sm-disable", "--indicator"]
  safeSpawnProg "blueman-applet"
  safeSpawn "synapse" ["-s"]

  liftIO $ findExecutable "ssh-askpass" >>= traverse_ (setEnv "SSH_ASKPASS")
  spawnOnce "ssh-add"
