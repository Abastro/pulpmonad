module StartHook where

import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Kind (Type)
import Data.Proxy
import Data.Set qualified as S
import Data.Typeable
import Defines
import System.Environment
import System.IO
import XMonad
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

modifyingM :: (ExtensionClass a) => (a -> X a) -> X ()
modifyingM f = XS.get >>= f >>= XS.put

newtype PerformOnce (t :: Type) = PerformOnce (S.Set String)
  deriving (Read, Show)

instance (Typeable t) => ExtensionClass (PerformOnce t) where
  initialValue = PerformOnce mempty
  extensionType = PersistentExtension

-- | Perform an action only once, alike `spawnOnce`. The proxy type should be unique.
performOnce :: forall (t :: Type). Typeable t => Proxy t -> X () -> X ()
performOnce proxy run =
  modifyingM @(PerformOnce t) $ fmap coerce . S.alterF noting desig . coerce
  where
    desig = show (typeOf proxy)
    noting b = True <$ unless b run

-- | Designate initiation.
data Initiate

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
initiatePrograms = performOnce (Proxy @Initiate) $ do
  liftIO $ hPrintf stderr "[Normal] Initiating programs... \n"
  safeSpawnProg "status-notifier-watcher"
  safeSpawnProg "pasystray"
  safeSpawn "nm-applet" ["--sm-disable", "--indicator"]
  safeSpawnProg "blueman-applet"
  safeSpawn "synapse" ["-s"]

  liftIO $ findExecutable "ssh-askpass" >>= traverse_ (setEnv "SSH_ASKPASS")
  spawnOnce "ssh-add"
