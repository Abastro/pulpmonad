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
import Pulp.Desk.System.X11.WMStatus

defImageIcon :: V.Vector T.Text -> Maybe (WindowInfo -> IO Gio.Icon)
defImageIcon _ = Nothing

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
