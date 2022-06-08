-- | System tray using status-notifier-item package.
-- Code extracted from UI-sni-tray, and then modified.
module System.Applet.SysTray (
  SysTrayArgs (..),
  systemTray,
) where

import System.Applet.SysTray.Handle

-- MAYBE I'd add my own status-notifier-item support
