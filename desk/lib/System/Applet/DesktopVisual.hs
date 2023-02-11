-- | Desktops visualizer widget.
module System.Applet.DesktopVisual (
  NumWindows,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
  defImageIcon,
  defShowFn,
) where

import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio.Interfaces.Icon qualified as Gio
import Status.X11.WMStatus
import System.Applet.DesktopVisual.Handle

defImageIcon :: V.Vector T.Text -> Maybe (WindowInfo -> IO Gio.Icon)
defImageIcon _ = Nothing

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
