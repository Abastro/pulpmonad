-- | Desktops visualizer widget.
module Pulp.Desk.Applet.DesktopVisual (
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
import Pulp.Desk.Applet.DesktopVisual.Handle
import Pulp.Desk.System.X11.WMStatus qualified as X11

defImageIcon :: V.Vector T.Text -> Maybe (X11.WindowInfo -> IO Gio.Icon)
defImageIcon _ = Nothing

defShowFn :: X11.DesktopStat -> NumWindows -> Bool
defShowFn X11.DesktopStat{desktopState} num = desktopState /= X11.DeskHidden || num > 0
