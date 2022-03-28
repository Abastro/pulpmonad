module Main (main) where

import Bar
import Defines
import System.FilePath
import System.Posix
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
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Util.Themes

scratchpads =
  [NS "term" "gnome-terminal --class=term-pers" (className =? "term-pers") defaultFloating]

myLayout =
  onWorkspaces ["code", "pics"] (tabbed shrinkText myTabCfg) $
    tall ||| wide ||| myTab
  where
    myTabCfg = (theme adwaitaDarkTheme) {decoHeight = 50}
    tall = Tall 1 (3 / 100) (1 / 2)
    wide = Mirror (Tall 1 (3 / 100) (1 / 2))
    myTab = tabbed shrinkText myTabCfg

staticManage =
  composeAll
    [ resource =? "synapse" --> doIgnore,
      className =? "Gimp" --> doF (shift "pics"),
      role =? "gimp-toolbox" <||> role =? "gimp-image-window" --> unFloat,
      className =? "zoom" <&&> (not <$> (title =? "Zoom" <||> title =? "Zoom Meeting")) --> doFloat,
      className =? "Soffice" <&&> isFullscreen --> doFullFloat,
      className =? "Gnome-calculator" --> doFloat,
      className =? "Eog" --> doFloat,
      className =? "Steam" --> doF (shift "pics"),
      className =? "kakaotalk.exe"
        <&&> (title =? "KakaoTalkEdgeWnd" <||> title =? "KakaoTalkShadowWnd") --> doIgnore,
      winTypeIs "_NET_WM_WINDOW_TYPE_DIALOG" --> doFloat
    ]

main :: IO ()
main = do
  dirs <- getDirectories
  let xmDir = cfgDir dirs
      cfg = ewmh desktopConfig
      mouseMove =
        [((controlMask, middleClick), \w -> isFloating w --> (focus w >> kill))]
      keysUtility =
        [ ("M-S-/", safeSpawn "eog" [xmDir </> "asset" </> "Xmbindings.png"]),
          ("M-d", safeSpawnProg "nautilus"),
          ("M-S-t", namedScratchpadAction scratchpads "term"),
          ("M-M1-s", safeSpawnProg "/usr/local/pulse/pulseUi")
        ]
      keysBasic =
        [ ("M-p", safeSpawnProg "synapse"),
          ("M-M1-<Delete>", safeSpawn "systemctl" ["poweroff"]),
          ("<XF86MonBrightnessUp>", safeSpawn "lux" ["-a", "5%"]),
          ("<XF86MonBrightnessDown>", safeSpawn "lux" ["-s", "5%"]),
          ("<XF86AudioRaiseVolume>", safeSpawn "pactl" ["set-sink-volume", "0", "+5%"]),
          ("<XF86AudioLowerVolume>", safeSpawn "pactl" ["set-sink-volume", "0", "-5%"]),
          ("<XF86AudioMute>", safeSpawn "pactl" ["set-sink-mute", "0", "toggle"])
        ]
      keysScreenshot =
        [ ("<Print>", spawn "sleep 0.2; gnome-screenshot"),
          ("M1-<Print>", spawn "sleep 0.2; gnome-screenshot -w"),
          ("C-<Print>", spawn "gnome-screenshot -i")
        ]

  safeSpawn "feh" ["--bg-scale", xmDir </> "asset" </> "Background.jpg"]
  killBar <- fmap (signalProcess killProcess) . forkProcess $ startBar xmDir
  let keysSpecial =
        [ ("M-M1-d", debugStack),
          ("M-q", io $ killBar >> spawn "xmonad --recompile && xmonad --restart")
        ]
  (`launch` dirs) . ewmhFullscreen . pagerHints $
    cfg
      { focusedBorderColor = "#eeaaaa",
        normalBorderColor = "#cccccc",
        workspaces = ["main", "docs", "code", "term", "chat", "pics", "7", "8", "9"],
        terminal = "gnome-terminal",
        startupHook =
          startupHook cfg <> do
            setWMName "LG3D"
            gnomeRegister -- Registers xmonad with gnome
            safeSpawnProg (xmDir </> "xmonad.hook"),
        manageHook = staticManage <> namedScratchpadManageHook scratchpads <> manageHook cfg,
        layoutHook = mouseResize . smartBorders . avoidStruts $ myLayout,
        handleEventHook = handleEventHook cfg,
        modMask = mod4Mask -- Super key
      }
      `additionalMouseBindings` mouseMove
      `additionalKeysP` concat [keysUtility, keysBasic, keysSpecial, keysScreenshot]
