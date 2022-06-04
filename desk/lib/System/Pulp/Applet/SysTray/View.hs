module System.Pulp.Applet.SysTray.View where

import Control.Monad.IO.Class
import GI.Gtk.Objects.Box qualified as UI
import UI.Commons qualified as UI

data SysTray = SysTray
  { sysTrayWid :: !UI.Widget
  , sysTrayBox :: !UI.Box
  }

sysTrayNew :: MonadIO m => UI.Orientation -> m SysTray
sysTrayNew orientation = do
  sysTrayBox <- UI.boxNew orientation 0
  sysTrayWid <- UI.toWidget sysTrayBox
  pure SysTray{..}


