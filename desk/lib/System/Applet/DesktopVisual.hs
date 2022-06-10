-- | Desktops visualizer widget.
module System.Applet.DesktopVisual (
  NumWindows,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
  defImageIcon,
  defShowFn,
) where

import Control.Monad.Trans.Maybe
import GI.Gio.Interfaces.Icon qualified as Gio
import Status.X11.WMStatus
import System.Applet.DesktopVisual.Handle

defImageIcon :: WindowInfo -> MaybeT IO Gio.Icon
defImageIcon = const (fail "not handle")

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
