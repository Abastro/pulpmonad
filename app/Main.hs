{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main ( main ) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers ( isFullscreen, doFullFloat )
import XMonad.Actions.MouseResize ( mouseResize )
import XMonad.Util.Run ( spawnPipe, safeSpawn, safeSpawnProg )
import XMonad.Util.EZConfig ( additionalKeys, additionalMouseBindings )
import XMonad.Config.Desktop ( desktopConfig )
import XMonad.Config.Gnome ( gnomeRegister )
import System.IO ( hPutStrLn )
import Text.Printf ( printf )

mkPath :: [FilePath] -> FilePath
mkPath = intercalate "/"
pathCfg, pathAs :: FilePath
pathCfg = mkPath ["$HOME", ".xmonad", "cfg"]
pathAs = mkPath ["$HOME", ".xmonad", "asset"]

(<-|) :: String -> [String] -> String
pr <-| opts = unwords (pr : opts)
(-:), (--:), (=:) :: String -> String -> String
opt --: val = printf "--%s %s" opt val
opt -: val = printf "-%s %s" opt val
opt =: val = printf "%s=\"%s\"" opt val


background, tray, statBar :: String
background = "feh" <-| ["bg-scale" --: mkPath [pathAs, "Background.jpg"]]
tray = "trayer" <-| [
    "--edge top", "--align right", "--expand true"
  , "--SetDockType true", "--SetPartialStrut true"
  , "--transparent true", "--alpha 0", "--tint 0x000000"
  , "--width 10", "--height 35"
  ]
statBar = "xmobar" <-| [ mkPath [pathCfg, ".xmobar"] ]

console, browser, logout :: String
console = "gnome-terminal"
browser = "nautilus"
logout = "gnome-session-quit"

leftClick = button1; rightClick = button3; middleClick = button2;
altMask = mod1Mask; superMask = mod4Mask

main :: IO ()
main = do
  spawn background
  spawn $ "killall trayer;" <> tray
  xmbar <- spawnPipe statBar
  let config = ewmh desktopConfig
  xmonad . ewmhFullscreen $ config {
    focusedBorderColor = "#eeaaaa"
  , normalBorderColor = "#cccccc"
  , workspaces = ["main", "side", "code", "term", "chat", "pic", "7", "8", "9"]
  , terminal = console
  , startupHook = startupHook config <+> do
      setWMName "LG3D"
      gnomeRegister -- Registers xmonad with gnome
      spawn $ mkPath ["$HOME", ".xmonad", "xmonad.hook"]
  , manageHook = composeAll $ [
      className =? "Gimp" --> doF (W.shift "pic")
    , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> unFloat
    , className =? "zoom" <&&> (not <$> (
      title =? "Zoom" <||> title =? "Zoom Meeting")) --> doFloat
    , className =? "Soffice" <&&> isFullscreen --> doFullFloat
    , className =? "Gnome-calculator" --> doFloat
    , className =? "Eog" --> doFloat
    , winTypeIs "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
    ] <> [ manageHook config ]
  , layoutHook = mouseResize $ layoutHook config
  , logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmbar
    , ppTitle  = xmobarColor "#555555" "" . shorten 50
    }
  , handleEventHook = handleEventHook config
  , modMask = superMask
  } `additionalMouseBindings` concat [ mouseMove ]
    `additionalKeys` concat [ keysUtility, keysBasic, keysScreenshot ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    winTypeIs typ = do
      w <- ask
      liftX . withDisplay $ \d -> do
        a <- getAtom "_NET_WM_WINDOW_TYPE"
        t <- getAtom typ
        long <- io $ getWindowProperty32 d a w
        pure $ t `elem` (fromIntegral <$> fromMaybe [] long)

      -- stringProperty "_NET_WM_WINDOW_TYPE"
    unFloat = ask >>= doF . W.sink
    isFloating = \w -> M.member w . W.floating <$> gets windowset

    mouseMove = [
        -- Mod(Super) + Left Click: Float & Move around
        -- Mod(Super) + Right Click: Float & Resize
        ((controlMask, middleClick), \w -> isFloating w --> (focus w >> kill))
      ]
    keysUtility = [
        ((superMask .|. altMask, xK_h), spawn $ "xdg-open" <-| [mkPath [pathAs, "Xmbindings.png"]])
      , ((superMask, xK_d), safeSpawnProg browser)
      , ((superMask .|. altMask, xK_s), spawn "/usr/local/pulse/pulseUi")
      ]
    keysBasic = [
        ((superMask, xK_p), spawn "dmenu_run")
      -- Super + Shift + Q: Logout
      , ((superMask .|. altMask, xK_Delete), spawn "systemctl poweroff")
      , ((noModMask, xF86XK_MonBrightnessUp), safeSpawn "lux" ["-a", "5%"])
      , ((noModMask, xF86XK_MonBrightnessDown), safeSpawn "lux" ["-s", "5%"])
      ]
    keysScreenshot = [
        ((noModMask, xK_Print), spawn "sleep 0.2; gnome-screenshot")
      , ((altMask, xK_Print), spawn "sleep 0.2; gnome-screenshot -w")
      , ((controlMask, xK_Print), spawn "gnome-screenshot -i")
      ]
