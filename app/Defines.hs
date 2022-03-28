module Defines
  ( role,
    winTypeIs,
    unFloat,
    isFloating,
    leftClick,
    rightClick,
    middleClick,
    W.shift
  )
where

import Data.Map qualified as M
import XMonad
import XMonad.StackSet qualified as W

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

winTypeIs :: String -> Query Bool
winTypeIs typ = do
  w <- ask
  liftX . withDisplay $ \d -> do
    a <- getAtom "_NET_WM_WINDOW_TYPE"
    t <- getAtom typ
    long <- io $ getWindowProperty32 d a w
    pure $ t `elem` maybe [] (map fromIntegral) long

unFloat :: ManageHook
unFloat = ask >>= doF . W.sink

isFloating :: Window -> X Bool
isFloating w = M.member w . W.floating <$> gets windowset

leftClick, rightClick, middleClick :: Button
(leftClick, rightClick, middleClick) = (button1, button3, button2)
