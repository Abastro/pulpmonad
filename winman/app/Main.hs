module Main (main) where

import Defines
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
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.StackSet (shift)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Util.Themes
import XMonad.Actions.GridSelect

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isTooltip :: Query Bool
isTooltip = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLTIP"

_leftClick, _rightClick, middleClick :: Button
(_leftClick, _rightClick, middleClick) = (button1, button3, button2)

main :: IO ()
main = do
  dirs <- getDirectories
  let xmDir = cfgDir dirs
      xmCache = cacheDir dirs

      onStart = do
        setWMName "LG3D"
        gnomeRegister -- Registers xmonad with gnome
        safeSpawn "feh" ["--bg-scale", xmDir </> "asset" </> "background.jpg"]

      pulpBar = statusBarGeneric (xmCache </> "pulp-taskbar") mempty

  xmonad . ewmhFullscreen . pagerHints . withSB pulpBar $
    cfg
      { focusedBorderColor = "#eeaaaa",
        normalBorderColor = "#cccccc",
        workspaces = mySpaces,
        terminal = "gnome-terminal",
        startupHook = onStart <> startupHook cfg,
        manageHook = manageHook cfg <> staticManage <> namedScratchpadManageHook scratchpads,
        layoutHook = lessBorders (Combine Union Never OnlyFloat) myLayout,
        handleEventHook = handleEventHook cfg <> minimizeEventHook,
        modMask = mod4Mask -- Super key
      }
      `additionalMouseBindings` mouseMove
      `additionalKeysP` concat [keysUtility xmDir, keysBasic xmCache, keysSpecial, keysScreenshot]
      `removeKeysP` keysRemoved
  where
    cfg = ewmh desktopConfig
    -- MAYBE keybindings to cfg file
    mouseMove =
      [((controlMask, middleClick), \w -> runQuery isFloating w --> (focus w >> kill))]
    keysUtility xmDir =
      [ ("M-S-/", safeSpawn "eog" [xmDir </> "asset" </> "xmbindings.png"]),
        ("M-d", safeSpawnProg "nautilus"),
        ("M-M1-t", namedScratchpadAction scratchpads (name scTerm))
      ]
    keysBasic xmCache =
      [ ("M-p", safeSpawnProg "synapse"),
        ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "5"]),
        ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "5"]),
        ("<XF86AudioRaiseVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+5%"]),
        ("<XF86AudioLowerVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-5%"]),
        ("<XF86AudioMute>", safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]),
        ("M-S-x", safeSpawnProg (xmCache </> "pulp-sysctl")),
        ("M-s", goToSelected gotoCfg)
      ]
    keysScreenshot =
      [ ("<Print>", spawn "sleep 0.2; gnome-screenshot"),
        ("M1-<Print>", spawn "sleep 0.2; gnome-screenshot -w"),
        ("C-<Print>", spawn "gnome-screenshot -i")
      ]
    keysSpecial =
      [("M-M1-d", debugStack)]
    keysRemoved =
      ["M-q", "M-S-q", "M-S-p"]

    gotoCfg =
      def
        { gs_bordercolor = "#404040"
        }

scTerm =
  NS "term" "gnome-terminal --role=term-pers" (role =? "term-pers") doCenterFloat

scratchpads = [scTerm]

myLayout =
  mouseResize . avoidStruts . minimize . maximize
    . onWorkspaces [code, game] myTab
    $ tall ||| wide ||| myTab
  where
    myTabCfg = (theme adwaitaDarkTheme) {decoHeight = 50}
    tall = Tall 1 (3 / 100) (1 / 2)
    wide = Mirror (Tall 1 (3 / 100) (1 / 2))
    -- Tabbed Left is not great for now. Gotta work my own later
    myTab = tabbed shrinkText myTabCfg

staticManage =
  composeAll
    [ isDialog --> doCenterFloat,
      isSplash --> doIgnore,
      isTooltip --> doIgnore,
      className =? "Gimp" --> doF (shift pics),
      role =? "gimp-toolbox" <||> role =? "gimp-image-window" --> doSink,
      className =? "Inkscape" --> doF (shift pics),
      className =? "zoom" <&&> (not <$> (title =? "Zoom" <||> title =? "Zoom Meeting")) --> doSideFloat CE,
      className =? "Soffice" <&&> isFullscreen --> doFullFloat,
      className =? "Gnome-calculator" --> doCenterFloat,
      className =? "Gnome-system-monitor" --> doCenterFloat,
      className =? "Gnome-control-center" --> doCenterFloat,
      className =? "term-float" --> doCenterFloat,
      className =? "Eog" --> doCenterFloat,
      className =? "Steam" --> doF (shift game),
      className =? "kakaotalk.exe"
        <&&> (title =? "KakaoTalkEdgeWnd" <||> title =? "KakaoTalkShadowWnd") --> doHideIgnore
    ]
