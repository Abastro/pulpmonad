-- | System tray using status-notifier-item package.
-- Code extracted from UI-sni-tray, and then modified.
module Pulp.Desk.Applet.SysTray (
  SysTrayArgs (..),
  systemTray,
) where

import Pulp.Desk.Applet.SysTray.Handle

-- MAYBE I'd add my own status-notifier-item support
