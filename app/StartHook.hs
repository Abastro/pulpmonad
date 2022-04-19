module StartHook where
import Defines
import XMonad
import XMonad.Util.SpawnOnce
import System.Environment
import System.Directory
import Data.Foldable

copyConfig :: FilePath -> IO ()
copyConfig mainDir = do
  getEnv "GTK2_RC_FILES" >>= \gtkrc -> do
    copyFile (mainDir </> "cfg" </> ".gtkrc-2.0") gtkrc

  getXdgDirectory XdgConfig "gtk-3.0" >>= \gtk3 -> do
    copyFile (mainDir </> "cfg" </> "settings.ini") (gtk3 </> "settings.ini")

initiatePrograms :: X ()
initiatePrograms = do
  spawnOnce "status-notifier-watcher"
  spawnOnce "pasystray"
  spawnOnce "nm-applet --sm-disable --indicator"
  spawnOnce "blueman-applet"
  spawnOnce "synapse -s"
  -- Environment variable setup can happen multiple times
  liftIO $ findExecutable "ssh-askpass" >>= traverse_ (setEnv "SSH_ASKPASS")
  spawnOnce "ssh-add"
