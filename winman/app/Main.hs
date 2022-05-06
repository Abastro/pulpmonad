module Main (main) where

import Defines
import Selects
import StartHook
import XMonad
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.DebugStack
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.StackSet (shift)
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Util.Themes

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

_leftClick, _rightClick, middleClick :: Button
(_leftClick, _rightClick, middleClick) = (button1, button3, button2)

scTerm =
  ( NS "term" "gnome-terminal --class=term-pers" (className =? "term-pers") doCenterFloat
  )

scratchpads = [scTerm]

myLayout =
  minimize . maximize
    . onWorkspaces [code, pics] myTab
    $ tall ||| wide ||| myTab
  where
    myTabCfg = (theme adwaitaDarkTheme) {decoHeight = 50}
    tall = Tall 1 (3 / 100) (1 / 2)
    wide = Mirror (Tall 1 (3 / 100) (1 / 2))
    -- Tabbed Left is not great for now. Gotta work my own later
    myTab = tabbed shrinkText myTabCfg

staticManage =
  composeAll
    [ resource =? "synapse" --> doIgnore,
      className =? "Gimp" --> doF (shift pics),
      role =? "gimp-toolbox" <||> role =? "gimp-image-window" --> doSink,
      className =? "zoom" <&&> (not <$> (title =? "Zoom" <||> title =? "Zoom Meeting")) --> doSideFloat CE,
      className =? "Soffice" <&&> isFullscreen --> doFullFloat,
      className =? "Gnome-calculator" --> doCenterFloat,
      className =? "Gnome-system-monitor" --> doCenterFloat,
      className =? "Gnome-control-center" --> doCenterFloat,
      className =? "term-float" --> doCenterFloat,
      className =? "Eog" --> doCenterFloat,
      className =? "Steam" --> doF (shift pics),
      className =? "kakaotalk.exe"
        <&&> (title =? "KakaoTalkEdgeWnd" <||> title =? "KakaoTalkShadowWnd") --> doIgnore,
      isDialog --> doFloat
    ]

main :: IO ()
main = do
  dirs <- getDirectories
  let xmDir = cfgDir dirs
      xmCache = cacheDir dirs
      cfg = ewmh desktopConfig
      mouseMove =
        [((controlMask, middleClick), \w -> runQuery isFloating w --> (focus w >> kill))]
      keysUtility =
        [ ("M-S-/", safeSpawn "eog" [xmDir </> "asset" </> "xmbindings.png"]),
          ("M-d", safeSpawnProg "nautilus"),
          ("M-M1-t", namedScratchpadAction scratchpads (name scTerm)),
          ("M-M1-s", safeSpawnProg "/usr/local/pulse/pulseUi")
        ]
      keysBasic =
        [ ("M-p", safeSpawnProg "synapse"),
          ("<XF86MonBrightnessUp>", safeSpawn "lux" ["-a", "5%"]),
          ("<XF86MonBrightnessDown>", safeSpawn "lux" ["-s", "5%"]),
          ("<XF86AudioRaiseVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+5%"]),
          ("<XF86AudioLowerVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-5%"]),
          ("<XF86AudioMute>", safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]),
          ("M-S-x", actSystemCtl sysCtlCfg dirs),
          ("M-s", actGotoWindow gotoCfg)
        ]
      keysScreenshot =
        [ ("<Print>", spawn "sleep 0.2; gnome-screenshot"),
          ("M1-<Print>", spawn "sleep 0.2; gnome-screenshot -w"),
          ("C-<Print>", spawn "gnome-screenshot -i")
        ]
      keysSpecial =
        [("M-M1-d", debugStack)]

      sysCtlCfg =
        def
          { ts_background = 0x02080808,
            ts_node = (0xffa0a0a0, 0xff282828),
            ts_nodealt = (0xffa0a0a0, 0xff2b2b2b),
            ts_highlight = (0xffb0b0b0, 0xff383838),
            ts_font = "xft:Sans-12"
          }
      gotoCfg =
        def
          { gs_bordercolor = "#404040"
          }

      onStart = do
        setWMName "LG3D"
        gnomeRegister -- Registers xmonad with gnome
        safeSpawn "feh" ["--bg-scale", xmDir </> "asset" </> "background.jpg"]
        copyConfig xmDir
        initiatePrograms

      pulpBar = statusBarGeneric (xmCache </> "pulpbar") mempty

  xmonad . ewmhFullscreen . pagerHints . withSB pulpBar $
    cfg
      { focusedBorderColor = "#eeaaaa",
        normalBorderColor = "#cccccc",
        workspaces = mySpaces,
        terminal = "gnome-terminal",
        startupHook = startupHook cfg <> onStart,
        manageHook = manageHook cfg <> staticManage <> namedScratchpadManageHook scratchpads,
        layoutHook = mouseResize . smartBorders . avoidStruts $ myLayout,
        handleEventHook = handleEventHook cfg <> minimizeEventHook,
        modMask = mod4Mask -- Super key
      }
      `additionalMouseBindings` mouseMove
      `additionalKeysP` concat [keysUtility, keysBasic, keysSpecial, keysScreenshot]
