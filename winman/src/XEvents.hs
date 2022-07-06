module XEvents where

import Control.Monad.Reader
import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import XMonad.Core

data XCtrlSend = XSysCtl | XWMCtl

-- | Sends custom global control message over X11.
xCtrlMsg :: XCtrlSend -> X ()
xCtrlMsg send = do
    disp <- asks display
    rWin <- asks theRoot
    ctrlTyp <- getAtom "_XMONAD_CTRL_MSG"
    contTyp <- getAtom (sendTyp send)
    io . allocaXEvent $ \ev -> do
      setEventType ev clientMessage
      setClientMessageEvent ev rWin ctrlTyp 32 contTyp currentTime
      sendEvent disp rWin False structureNotifyMask ev
  where
    sendTyp = \case
      XSysCtl -> "_XMONAD_CTRL_SYS"
      XWMCtl -> "_XMONAD_CTRL_WM"
