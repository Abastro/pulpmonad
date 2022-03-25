{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.List
import Data.Map qualified as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import Text.Printf (printf)
import XMonad
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.DebugStack
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Util.Themes

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
tray =
  "trayer"
    <-| [ "--edge top",
          "--align right",
          "--expand true",
          "--SetDockType true",
          "--SetPartialStrut true",
          "--transparent true",
          "--alpha 0",
          "--tint 0x000000",
          "--width 10",
          "--height 35"
        ]
statBar = "xmobar" <-| [mkPath [pathCfg, ".xmobar"]]

console, browser, logout :: String
console = "gnome-terminal"
browser = "nautilus"
logout = "gnome-session-quit"

leftClick, rightClick, middleClick :: Button
(leftClick, rightClick, middleClick) = (button1, button3, button2)

altMask, superMask :: KeyMask
(altMask, superMask) = (mod1Mask, mod4Mask)

myTabCfg = (theme adwaitaDarkTheme) {decoHeight = 50}

myLayout =
  onWorkspaces ["code", "pics"] (tabbed shrinkText myTabCfg) $
    tall ||| wide ||| myTab
  where
    tall = Tall 1 (3 / 100) (1 / 2)
    wide = Mirror (Tall 1 (3 / 100) (1 / 2))
    myTab = tabbed shrinkText myTabCfg

main :: IO ()
main = do
  spawn background
  let config = ewmh desktopConfig
  xmonad . ewmhFullscreen . pagerHints $
    config
      { focusedBorderColor = "#eeaaaa",
        normalBorderColor = "#cccccc",
        workspaces = ["main", "docs", "code", "term", "chat", "pics", "7", "8", "9"],
        terminal = console,
        startupHook =
          startupHook config <+> do
            setWMName "LG3D"
            gnomeRegister -- Registers xmonad with gnome
            spawn $ mkPath ["$HOME", ".xmonad", "xmonad.hook"],
        manageHook = staticManage <> manageHook config,
        layoutHook = mouseResize . smartBorders . avoidStruts $ myLayout,
        handleEventHook = handleEventHook config,
        modMask = superMask
      }
      `additionalMouseBindings` mouseMove
      `additionalKeys` concat [keysUtility, keysBasic, keysSpecial, keysScreenshot]
  where
    staticManage =
      composeAll
        [ resource =? "synapse" --> doIgnore,
          className =? "Gimp" --> doF (W.shift "pics"),
          (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> unFloat,
          className =? "zoom" <&&> (not <$> (title =? "Zoom" <||> title =? "Zoom Meeting")) --> doFloat,
          className =? "Soffice" <&&> isFullscreen --> doFullFloat,
          className =? "Gnome-calculator" --> doFloat,
          className =? "Eog" --> doFloat,
          className =? "Steam" --> doF (W.shift "pics"),
          className =? "kakaotalk.exe"
            <&&> (title =? "KakaoTalkEdgeWnd" <||> title =? "KakaoTalkShadowWnd") --> doIgnore,
          winTypeIs "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
        ]

    role = stringProperty "WM_WINDOW_ROLE"
    winTypeIs typ = do
      w <- ask
      liftX . withDisplay $ \d -> do
        a <- getAtom "_NET_WM_WINDOW_TYPE"
        t <- getAtom typ
        long <- io $ getWindowProperty32 d a w
        pure $ t `elem` (fromIntegral <$> fromMaybe [] long)

    unFloat = ask >>= doF . W.sink
    isFloating = \w -> M.member w . W.floating <$> gets windowset

    mouseMove =
      [ -- Mod(Super) + Left Click: Float & Move around
        -- Mod(Super) + Right Click: Float & Resize
        ((controlMask, middleClick), \w -> isFloating w --> (focus w >> kill))
      ]
    keysUtility =
      [ ((superMask .|. altMask, xK_h), spawn $ "xdg-open" <-| [mkPath [pathAs, "Xmbindings.png"]]),
        ((superMask, xK_d), safeSpawnProg browser),
        ((superMask .|. altMask, xK_s), spawn "/usr/local/pulse/pulseUi")
      ]
    keysBasic =
      [ ((superMask, xK_p), spawn "synapse"),
        -- Super + Shift + Q: Logout
        ((superMask .|. altMask, xK_Delete), spawn "systemctl poweroff"),
        ((noModMask, xF86XK_MonBrightnessUp), safeSpawn "lux" ["-a", "5%"]),
        ((noModMask, xF86XK_MonBrightnessDown), safeSpawn "lux" ["-s", "5%"]),
        ((noModMask, xF86XK_AudioRaiseVolume), safeSpawn "pactl" ["set-sink-volume", "0", "+5%"]),
        ((noModMask, xF86XK_AudioLowerVolume), safeSpawn "pactl" ["set-sink-volume", "0", "-5%"]),
        ((noModMask, xF86XK_AudioMute), safeSpawn "pactl" ["set-sink-mute", "0", "toggle"])
      ]
    keysScreenshot =
      [ ((noModMask, xK_Print), spawn "sleep 0.2; gnome-screenshot"),
        ((altMask, xK_Print), spawn "sleep 0.2; gnome-screenshot -w"),
        ((controlMask, xK_Print), spawn "gnome-screenshot -i")
      ]
    keysSpecial =
      [((superMask .|. altMask, xK_d), debugStack)]
