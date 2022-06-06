-- | System tray using status-notifier-item package.
-- Code extracted from UI-sni-tray, and then modified.
module System.Pulp.Applet.SysTray (
  SysTrayArgs (..),
  systemTray,
) where

import System.Pulp.Applet.SysTray.Handle

-- MAYBE I'd add my own status-notifier-item support
