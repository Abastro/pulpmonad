{-# LANGUAGE OverloadedLabels #-}

module System.Applet.DesktopVisual.View (
  widgetUpdateClass,
  MainView (..),
  mainView,
  DeskItemView (..),
  deskItemView,
  WinItemView (..),
  winItemView,
) where

import Control.Event.Entry
import Control.Monad.IO.Class
import Data.Foldable
import Data.GI.Base.Attributes
import Data.GI.Base.Constructible
import Data.Text qualified as T
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gdk.Unions.Event qualified as Gdk
import GI.Gio.Interfaces.Icon qualified as Gio
import GI.Gtk.Objects.Box qualified as Gtk
import GI.Gtk.Objects.FlowBox qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk
import Gtk.Pixbufs qualified as Gtk
import Gtk.Styles qualified as Gtk
import Status.X11.WMStatus
import System.FilePath
import System.Pulp.PulpEnv

widgetUpdateClass :: (Enum s, Bounded s, MonadIO m) => Gtk.Widget -> (s -> T.Text) -> [s] -> m ()
widgetUpdateClass widget asClass state =
  #getStyleContext widget >>= Gtk.updateCssClass asClass state

data MainView = MainView
  { mainWidget :: !Gtk.Widget
  , mainAddDesktop :: Sink DeskItemView
  , mainRemoveDesktop :: Sink DeskItemView
  }

-- For now, main desktop visualizer is simply box of desktops

mainView :: MonadIO m => m MainView
mainView = do
  mainBox <- new Gtk.Box [#spacing := 5]
  #getStyleContext mainBox >>= flip #addClass (T.pack "desk-visual")

  mainWidget <- Gtk.toWidget mainBox
  let mainAddDesktop DeskItemView{deskWidget} = do
        #add mainBox deskWidget
        #showAll deskWidget
      mainRemoveDesktop DeskItemView{deskWidget} = do
        #hide deskWidget
        #remove mainBox deskWidget
  pure MainView{..}

-- We will have 'Event WinItemView' - the view is held.
-- No need to hold tree of views

data DeskItemView = DeskItemView
  { deskWidget :: !Gtk.Widget
  , deskSetName :: Sink T.Text
  , deskSetVisible :: Sink Bool
  , deskSetState :: Sink DesktopState
  , deskAddWindow :: Sink (Int, Gtk.Widget)
  , deskRemoveWindow :: Sink Gtk.Widget
  , deskReorderWindows :: Sink [Gtk.Widget]
  , deskActivates :: Source ()
  }

deskCssClass :: DesktopState -> T.Text
deskCssClass = \case
  DeskActive -> T.pack "active"
  DeskVisible -> T.pack "visible"
  DeskHidden -> T.pack "hidden"

-- TODO Change to use flowbox
deskItemView :: (MonadIO m, MonadPulpPath m) => m DeskItemView
deskItemView = pulpDataPath ("ui" </> "desk-item.ui") >>= liftIO . view
  where
    view uiFile = Gtk.buildFromFile (T.pack uiFile) $ do
      Just deskWidget <- Gtk.getElement (T.pack "desktop-item") Gtk.Widget
      Just deskLabel <- Gtk.getElement (T.pack "desktop-label") Gtk.Label
      -- Just deskCo <- Gtk.getElement (T.pack "desktop-container") Gtk.FlowBox
      Just deskCont <- Gtk.getElement (T.pack "desktop-container") Gtk.Box

      -- ListStore with my own objects?
      -- Window view should not be created by desktop view.
      -- Thus, having ListModel which creates widget does not make sense.
      -- Extending window item widget (accessible by FlowBoxChild) it is (include order).

      let deskSetName name = set deskLabel [#label := name]
          deskSetVisible flag = if flag then #showAll deskWidget else #hide deskWidget
          deskSetState state = widgetUpdateClass deskWidget deskCssClass [state]

          -- Handler will make sure child windows to be in sync
          deskAddWindow (idx, win) = do
            #add deskCont win
            #reorderChild deskCont win (fromIntegral idx)
            #showAll win
          deskRemoveWindow :: Sink Gtk.Widget
          deskRemoveWindow win = do
            #hide win
            #remove deskCont win
          deskReorderWindows wins = do
            for_ ([0 ..] `zip` wins) $ \(idx, win) -> do
              #reorderChild deskCont win (fromIntegral idx)

      (deskActivates, onRelease) <- liftIO sourceSink
      Gtk.addCallbackWithEvent (T.pack "desktop-click") Gdk.getEventButton $ handleRelease onRelease

      pure DeskItemView{..}

    handleRelease :: Sink () -> Sink Gdk.EventButton
    handleRelease callback event =
      get event #button >>= \case
        1 -> callback ()
        _ -> pure ()

data WinItemView = WinItemView
  { winWidget :: !Gtk.Widget
  , winSetTitle :: Sink T.Text
  , winSetGIcon :: Sink Gio.Icon
  , winSetRawIcons :: Sink [Gtk.RawIcon]
  , winActivates :: Source ()
  }

winItemView :: (MonadIO m, MonadPulpPath m) => m WinItemView
winItemView = pulpDataPath ("ui" </> "window-item.ui") >>= liftIO . view
  where
    view uiFile = Gtk.buildFromFile (T.pack uiFile) $ do
      Just winWidget <- Gtk.getElement (T.pack "window-item") Gtk.Widget
      Just winIcon <- Gtk.getElement (T.pack "window-item-icon") Gtk.Image
      let winSetTitle title = set winWidget [#tooltipText := title]
          winSetGIcon icon = set winIcon [#gicon := icon]
          winSetRawIcons icons = do
            -- Does not change icon size
            iconSize <- toEnum . fromIntegral <$> get winIcon #iconSize
            Gtk.iconsChoosePixbuf (Gtk.iconSizePx iconSize) Gtk.argbTorgba icons >>= \case
              Just pixbuf -> set winIcon [#pixbuf := pixbuf]
              -- No pixbuf to choose, default to missing
              Nothing -> set winIcon [#iconName := T.pack "image-missing"]

      (winActivates, onClick) <- liftIO sourceSink
      Gtk.addCallback (T.pack "window-item-activate") $ onClick ()
      pure WinItemView{..}
