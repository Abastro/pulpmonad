module Main (main) where

import Data.Proxy
import Defines
import StartHook
import System.Environment
import XEvents
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Config.Gnome (gnomeRegister)
import XMonad.Hooks.DebugStack
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDebug
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, safeSpawnProg)
import XMonad.Util.Themes

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
  let xmData = dataDir dirs
      xmCfg = cfgDir dirs

  let onStart = do
        setWMName "LG3D"
        gnomeRegister -- Registers xmonad with gnome
        safeSpawn "feh" ["--bg-scale", xmData </> "asset" </> "background.jpg"]
        performOnce (Proxy @Initiate) $ do
          safeSpawnProg (xmCfg </> "hook.sh")

      setupEnvs = do
        home <- liftIO getHomeDirectory
        liftIO $ setEnv "GTK2_RC_FILES" (home </> ".config" </> "gtk-2.0" </> ".gtkrc-2.0")

  xmonad . pagerHints $
    cfg
      { focusedBorderColor = "#eeaaaa",
        normalBorderColor = "#cccccc",
        borderWidth = 2,
        workspaces = mySpaces,
        terminal = "gnome-terminal",
        startupHook = setupEnvs <> onStart <> startupHook cfg,
        manageHook = namedScratchpadManageHook scratchpads <> manageStates <> manageSend <> manageHook cfg,
        layoutHook = lessBorders (Combine Union Never OnlyFloat) myLayout,
        handleEventHook = handleEventHook cfg <> minimizeEventHook,
        modMask = mod4Mask -- Super key
      }
      `additionalMouseBindings` mouseMove
      `additionalKeysP` concat [keysUtility xmData, keysBasic, keysSpecial, keysScreenshot]
      `removeKeysP` keysRemoved
 where
  cfg = debugManageHookOn "M-S-b" (ewmhFullscreen desktopConfig)
  -- MAYBE keybindings to cfg file
  mouseMove =
    [((controlMask, middleClick), \w -> runQuery isFloating w --> (focus w >> kill))]
  keysUtility xmData =
    [ ("M-S-/", safeSpawn "eog" [xmData </> "asset" </> "xmbindings.png"]),
      ("M-d", safeSpawnProg "nemo"),
      ("M-M1-t", namedScratchpadAction scratchpads (name scTerm))
    ]
  keysBasic =
    [ ("M-p", safeSpawnProg "synapse"),
      ("<XF86MonBrightnessUp>", safeSpawn "brightnessctl" ["-c", "backlight", "s", "+5%"]),
      ("<XF86MonBrightnessDown>", safeSpawn "brightnessctl" ["-c", "backlight", "s", "5%-"]),
      ("<XF86AudioRaiseVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+5%"]),
      ("<XF86AudioLowerVolume>", safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-5%"]),
      ("<XF86AudioMute>", safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]),
      ("M-x", xCtrlMsg XWMCtl),
      ("M-S-x", xCtrlMsg XSysCtl),
      ("M-<Tab>", goToSelected gotoCfg)
    ]
  keysScreenshot =
    [ ("<Print>", spawn "sleep 0.2; gnome-screenshot"),
      ("M1-<Print>", spawn "sleep 0.2; gnome-screenshot -w"),
      ("C-<Print>", spawn "gnome-screenshot -i")
    ]
  keysSpecial =
    [("M-M1-d", debugStack)]
  keysRemoved =
    ["M-q", "M-S-p"]

  gotoCfg =
    def
      { gs_bordercolor = "#404040",
        gs_cellwidth = 240,
        gs_cellheight = 80,
        gs_cellpadding = 20,
        gs_font = "xft:Sans-10"
      }

scTerm =
  NS "term" "gnome-terminal --role=term-pers" (role =? "term-pers") doCenterFloat

scratchpads = [scTerm]

myLayout =
  mouseResize
    . avoidStruts
    . onWorkspace game mTab
    . onWorkspace code (mTab ||| mPanes)
    $ mTall ||| mTab ||| mPanes
 where
  mTall = renamed [Replace "Tall"] $ minMax tall
  mTab = renamed [Replace "Tabs"] $ minMax tabs
  mPanes = renamed [Replace "Panes"] $ minMax twoPane
  minMax layout = minimize (maximize layout)

  myTabCfg = (theme adwaitaDarkTheme){decoHeight = 50}
  tall = Tall 1 (3 / 100) (1 / 2)
  tabs = tabbed shrinkText myTabCfg
  twoPane = TwoPane (3 / 100) (1 / 2)

-- Current offender: Nautilus & Gnome-calculator
manageStates =
  composeOne
    [ isDialog -?> doCenterFloat,
      isSplash -?> doIgnore,
      isTooltip -?> doIgnore,
      role =? "popup" <||> role =? "pop-up" -?> doCenterFloat,
      role =? "gimp-toolbox" <||> role =? "gimp-image-window" -?> doSink,
      -- zoom be zoom with "zoom "
      (className =? "zoom" <||> className =? "zoom ")
        <&&> (not <$> (title =? "Zoom" <||> title =? "Zoom Meeting"))
        -?> doSideFloat CE,
      appName =? "soffice" <&&> isFullscreen -?> doFullFloat,
      appName =? "gnome-calculator" -?> doCenterFloat,
      appName =? "gnome-system-monitor" -?> doCenterFloat,
      appName =? "gnome-control-center" -?> doCenterFloat,
      appName =? "term-float" -?> doCenterFloat,
      appName =? "eog" -?> doCenterFloat,
      className
        =? "kakaotalk.exe"
        <&&> (title =? "KakaoTalkEdgeWnd" <||> title =? "KakaoTalkShadowWnd")
        -?> doHideIgnore
    ]

manageSend =
  composeOne
    [ className =? "Gimp" -?> doShift pics,
      appName =? "org.inkscape.Inkscape" -?> doShift pics,
      className =? "steam" -?> doShift game
    ]
