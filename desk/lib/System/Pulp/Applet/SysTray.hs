-- | System tray using status-notifier-item package.
-- Code extracted from UI-sni-tray, and then modified.
module System.Pulp.Applet.SysTray where

import Control.Concurrent.MVar
import Control.Monad
import DBus
import DBus.Client
import Data.Bool
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import GI.DbusmenuGtk3.Objects.Menu qualified as DMenu
import GI.Gdk.Flags qualified as Gdk
import GI.Gdk.Structs.EventButton qualified as Gdk
import GI.Gdk.Structs.EventScroll qualified as Gdk
import GI.Gdk.Structs.Rectangle qualified as Gdk
import GI.Gtk.Objects.Box qualified as UI
import GI.Gtk.Objects.EventBox qualified as UI
import GI.Gtk.Objects.Menu qualified as UI
import StatusNotifier.Host.Service
import StatusNotifier.Item.Client
import Text.Printf
import UI.Commons qualified as UI
import UI.Containers qualified as UI
import UI.Styles qualified as UI

data SysTrayArgs = SysTrayArgs
  { trayOrientation :: !UI.Orientation
  , trayIconSize :: !UI.IconSize
  , -- | Whether to align from the beginning
    trayAlignBegin :: !Bool
  }

-- MAYBE option to fit icon size to be uniform
sysTrayMake :: Host -> Client -> SysTrayArgs -> IO UI.Widget
sysTrayMake
  Host
    { itemInfoMap = getInfoMap
    , addUpdateHandler = addUHandler
    , removeUpdateHandler = removeUHandler
    }
  client
  SysTrayArgs{..} = do
    trayBox <- UI.boxNew trayOrientation 0
    contextMap <- newMVar M.empty

    let getContext name = (M.!? name) <$> readMVar contextMap
        showInfo info = show info{iconPixmaps = []}

        getSize rectangle =
          case trayOrientation of
            UI.OrientationHorizontal ->
              Gdk.getRectangleHeight rectangle
            UI.OrientationVertical ->
              Gdk.getRectangleWidth rectangle
            _ -> error "TODO"

        getInfoAttr fn def name = maybe def fn . (M.!? name) <$> getInfoMap

        getInfo :: ItemInfo -> BusName -> IO ItemInfo
        getInfo = getInfoAttr id

        updateIconFromInfo info@ItemInfo{itemServiceName = name} =
          getContext name >>= updateIcon
          where
            updateIcon Nothing = updateHandler ItemAdded info
            updateIcon (Just _) = do
              undefined -- TODO Set image
        getTooltipText ItemInfo{itemToolTip = Just (_, _, titleText, fullText)}
          | titleText == fullText = fullText
          | titleText == "" = fullText
          | fullText == "" = titleText
          | otherwise = printf "%s: %s" titleText fullText
        getTooltipText _ = ""

        setTooltipText :: UI.Widget -> ItemInfo -> IO ()
        setTooltipText widget info =
          UI.widgetSetTooltipText widget $ Just $ T.pack $ getTooltipText info

        updateHandler
          ItemAdded
          info@ItemInfo
            { menuPath = pathForMenu
            , itemServiceName = serviceName
            , itemServicePath = servicePath
            } =
            do
              let serviceNameStr = formatBusName serviceName
                  servicePathStr = formatObjectPath servicePath :: String
                  serviceMenuPathStr = formatObjectPath <$> pathForMenu

              button <- UI.eventBoxNew
              UI.widgetAddEvents button [Gdk.EventMaskScrollMask]

              image :: UI.Widget <- undefined -- use pixbuf
              UI.widgetGetStyleContext image
                >>= flip UI.styleContextAddClass (T.pack "tray-icon-image")

              UI.containerAdd button image
              setTooltipText (error "cast" button) info

              maybeMenu <-
                sequenceA $
                  DMenu.menuNew (T.pack serviceNameStr) . T.pack <$> serviceMenuPathStr

              let context = undefined
                  popupItemForMenu menu =
                    UI.menuPopupAtWidget
                      menu
                      image
                      UI.GravitySouthWest
                      UI.GravityNorthWest
                      Nothing

              _ <- UI.onWidgetButtonPressEvent button $ \event -> do
                button <- Gdk.getEventButtonButton event
                x <- round <$> Gdk.getEventButtonXRoot event
                y <- round <$> Gdk.getEventButtonYRoot event
                action <- case button of
                  1 ->
                    bool (error "leftClickAction") (error "PopupMenu")
                      <$> getInfoAttr itemIsMenu True serviceName
                  2 -> error "middleClickAction"
                  _ -> error "rightClickAction"
                {-
                case action of
                  Activate -> void $ activate client serviceName servicePath x y
                  SecondaryActivate ->
                    void $
                      secondaryActivate
                        client
                        serviceName
                        servicePath
                        x
                        y
                  PopupMenu -> maybe (return ()) popupItemForMenu maybeMenu
                -}
                return False
              _ <- UI.onWidgetScrollEvent button $ \event -> do
                direction <- Gdk.getEventScrollDirection event
                let direction' = case direction of
                      UI.ScrollDirectionUp -> Just "vertical"
                      UI.ScrollDirectionDown -> Just "vertical"
                      UI.ScrollDirectionLeft -> Just "horizontal"
                      UI.ScrollDirectionRight -> Just "horizontal"
                      _ -> Nothing
                    -- deltaX/deltaY are provided only in case of smooth scrolling which
                    -- is enabled via additional flag, we don't to enable/handle it
                    delta = case direction of
                      UI.ScrollDirectionUp -> -1
                      UI.ScrollDirectionDown -> 1
                      UI.ScrollDirectionLeft -> -1
                      UI.ScrollDirectionRight -> 1
                      _ -> 0
                traverse_ (scroll client serviceName servicePath delta) direction'
                return False

              modifyMVar_ contextMap $ return . M.insert serviceName context

              UI.widgetShowAll button
              let packFn = if trayAlignBegin then UI.boxPackStart else UI.boxPackEnd
              packFn trayBox button (error "shouldExpand") True 0
        updateHandler ItemRemoved ItemInfo{itemServiceName = name} =
          getContext name >>= removeWidget
          where
            removeWidget Nothing = undefined
            removeWidget (Just _) =
              do
                UI.containerRemove trayBox (error "widgetToRemove" :: UI.Widget)
                modifyMVar_ contextMap $ return . M.delete name
        updateHandler IconUpdated i = updateIconFromInfo i
        updateHandler OverlayIconUpdated i = updateIconFromInfo i
        updateHandler ToolTipUpdated info@ItemInfo{itemServiceName = name} =
          void $
            getContext name
              >>= traverse (flip setTooltipText info . error "contextButton")
        updateHandler _ _ = return ()

        {-
        maybeAddOverlayToPixbuf size info pixbuf = do
          runMaybeT $ do
            let overlayHeight = floor (fromIntegral size * overlayScale)
            overlayPixbuf <-
              MaybeT $
                getOverlayPixBufFromInfo overlayHeight info
                  >>= traverse (scalePixbufToSize overlayHeight UI.OrientationHorizontal)
            lift $ do
              actualOHeight <- getPixbufHeight overlayPixbuf
              actualOWidth <- getPixbufWidth overlayPixbuf
              mainHeight <- getPixbufHeight pixbuf
              mainWidth <- getPixbufWidth pixbuf
              pixbufComposite
                overlayPixbuf
                pixbuf
                0
                0 -- Top left corner
                actualOWidth
                actualOHeight -- Overlay size
                0
                0 -- Offset
                1.0
                1.0 -- Scale
                InterpTypeBilinear -- InterpType
                255 -- Source image alpha
          return pixbuf
        -}

        {-
        getScaledPixBufFromInfo size info =
          getPixBufFromInfo size info
            >>= traverse
              ( scalePixbufToSize size orientation
                  >=> maybeAddOverlayToPixbuf size info
              )
        -}

        getPixBufFromInfo
          size
          info@ItemInfo
            { iconName = name
            , iconThemePath = mpath
            , iconPixmaps = pixmaps
            } = getPixBufFrom size name mpath pixmaps

        getOverlayPixBufFromInfo
          size
          info@ItemInfo
            { overlayIconName = name
            , iconThemePath = mpath
            , overlayIconPixmaps = pixmaps
            } =
            getPixBufFrom
              size
              (fromMaybe "" name)
              mpath
              pixmaps

        getPixBufFrom size name mpath pixmaps = do
          let tooSmall (w, h, _) = w < size || h < size
              largeEnough = filter (not . tooSmall) pixmaps
              orderer (w1, h1, _) (w2, h2, _) =
                case compare w1 w2 of
                  EQ -> compare h1 h2
                  a -> a
              selectedPixmap =
                if null largeEnough
                  then maximumBy orderer pixmaps
                  else minimumBy orderer largeEnough
              getFromPixmaps (w, h, p) =
                if BS.length p == 0
                  then Nothing
                  else Just $ error "getIconPixbufFromByteString w h p"
          if null pixmaps
            then error "getIconPixbufByName size (T.pack name) mpath"
            else sequenceA $ getFromPixmaps selectedPixmap

        uiUpdateHandler updateType info =
          void $
            undefined
            {- Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT -} $
              updateHandler updateType info >> return False

    handlerId <- addUHandler uiUpdateHandler
    _ <- UI.onWidgetDestroy trayBox $ removeUHandler handlerId
    UI.toWidget trayBox
