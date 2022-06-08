-- | Desktops visualizer widget.
module System.Applet.DesktopVisual (
  ImageSet (..),
  NumWindows,
  DesktopSetup (..),
  WindowSetup (..),
  deskVisualizer,
  defImageSetter,
  defShowFn,
) where

import Control.Monad.Trans.Maybe
import Status.X11.WMStatus
import System.Applet.DesktopVisual.Handle
import View.Imagery

defImageSetter :: WindowInfo -> MaybeT IO ImageSet
defImageSetter = const (fail "not handle")

defShowFn :: DesktopStat -> NumWindows -> Bool
defShowFn DesktopStat{desktopState} num = desktopState /= DeskHidden || num > 0
