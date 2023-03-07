{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bar with image in the background.
--
-- Exists since Gtk does not allow scaling by proportion,
-- which is required to place bar at specific part of the icon.
module Pulp.Desk.UI.Widget.ImageBar (
  ImageBar (..),
  setupPos,
  setFill,
  setIcon,
) where

import Control.Monad
import Data.GI.Base qualified as GI
import Data.GI.Base.Overloading qualified as GI
import Data.GI.Base.ShortPrelude qualified as GI
import Data.IORef
import Data.Text qualified as T
import GHC.OverloadedLabels
import GHC.Records
import GI.Cairo.Render qualified as Cairo
import GI.Cairo.Render.Connector qualified as Cairo
import GI.Cairo.Structs.Context qualified as Cairo
import GI.Gdk.Functions qualified as Gdk
import GI.Gio.Interfaces.File qualified as Gio
import GI.Gtk.Objects.Bin qualified as Gtk
import GI.Gtk.Objects.Image qualified as Gtk
import GI.Gtk.Structs.WidgetClass qualified as Gtk
import Graphics.X11.Xlib.Types (Rectangle (..))
import Pulp.Desk.PulpPath
import Pulp.Desk.UI.Commons qualified as Gtk

-- MAYBE Figure out how to attach label for attributes. This is not crucial, though.

newtype ImageBar = AsImageBar (GI.ManagedPtr ImageBar)

instance GI.TypedObject ImageBar where
  glibType :: IO GI.GType
  glibType = GI.registerGType AsImageBar
instance GI.GObject ImageBar

-- MAYBE Add orientable? Sounds hard
type instance GI.ParentTypes ImageBar = Gtk.Bin ': GI.ParentTypes Gtk.Bin
instance GI.HasParentTypes ImageBar

data Private = MkPrivate
  { barOrientation :: !Gtk.Orientation
  , barScale :: !Word
  -- ^ Scale for the portion.
  , barPortion :: !Rectangle
  -- ^ Portion where bar is displayed. left - top - width - height.
  , barFill :: !(IORef Double)
  , barIcon :: !Gtk.Image
  }

-- MAYBE Export portion to CSS?

type instance GI.AttributeList ImageBar = GI.AttributeList Gtk.Widget
instance GI.HasAttributeList ImageBar
type instance GI.SignalList ImageBar = GI.SignalList Gtk.Widget
instance
  ( info ~ Gtk.ResolveWidgetMethod t ImageBar
  , GI.OverloadedMethod info ImageBar p
  ) =>
  IsLabel t (ImageBar -> p)
  where
  fromLabel = GI.overloadedMethod @info
instance
  ( info ~ Gtk.ResolveWidgetMethod t ImageBar
  , GI.OverloadedMethod info ImageBar p
  , HasField t ImageBar p
  ) =>
  HasField t ImageBar p
  where
  getField = GI.overloadedMethod @info

instance GI.DerivedGObject ImageBar where
  type GObjectParentType ImageBar = Gtk.Bin
  type GObjectPrivateData ImageBar = Private

  objectTypeName :: T.Text
  objectTypeName = T.pack "ImageBar"

  objectClassInit :: GI.GObjectClass -> IO ()
  objectClassInit = classInit

  objectInstanceInit :: GI.GObjectClass -> ImageBar -> IO (GI.GObjectPrivateData ImageBar)
  objectInstanceInit = instanceInit

classInit :: GI.GObjectClass -> IO ()
classInit gClass = Gtk.withClassAs Gtk.WidgetClass gClass $ \widgetClass -> do
  uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "image-bar.ui")
  Gtk.setTemplateFromGFile widgetClass uiFile

  widgetClass.bindTemplateChildFull (T.pack "bar-foreground") True 0

  -- Waiting for proper support for Enums here
  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { -- Poor man's Orientable
        name = T.pack "orient"
      , nick = T.pack "Orientation"
      , blurb = T.pack "Orientation of the bar"
      , defaultValue = 0
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barOrientation = (toEnum . fromIntegral) v}
      , getter = \widget -> do
          (\priv -> (fromIntegral . fromEnum) priv.barOrientation) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 1
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_scale"
      , nick = T.pack "Bar Scale"
      , blurb = T.pack "Scale for the portion specification"
      , defaultValue = 1
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barScale = fromIntegral v}
      , getter = \widget -> do
          (\priv -> fromIntegral priv.barScale) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 1
      , maxValue = Just 256
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_portion_left"
      , nick = T.pack "Portion Left"
      , blurb = T.pack "Left spacing of the portion"
      , defaultValue = 0
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = dat.barPortion{rect_x = fromIntegral v}}
      , getter = \widget -> do
          (\priv -> (fromIntegral . rect_x) priv.barPortion) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_portion_top"
      , nick = T.pack "Portion Top"
      , blurb = T.pack "Top spacing of the portion"
      , defaultValue = 0
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = dat.barPortion{rect_y = fromIntegral v}}
      , getter = \widget -> do
          (\priv -> (fromIntegral . rect_y) priv.barPortion) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_portion_width"
      , nick = T.pack "Portion Width"
      , blurb = T.pack "Width of the portion"
      , defaultValue = 0
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = dat.barPortion{rect_width = fromIntegral v}}
      , getter = \widget -> do
          (\priv -> (fromIntegral . rect_width) priv.barPortion) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_portion_height"
      , nick = T.pack "Portion Height"
      , blurb = T.pack "Height of the portion"
      , defaultValue = 0
      , setter = \widget v -> GI.gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = dat.barPortion{rect_height = fromIntegral v}}
      , getter = \widget -> do
          (\priv -> (fromIntegral . rect_height) priv.barPortion) <$> GI.gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  GI.gobjectInstallCStringProperty @ImageBar
    gClass
    GI.CStringPropertyInfo
      { name = T.pack "bar_icon_name"
      , nick = T.pack "Bar Icon Name"
      , blurb = T.pack "Name of the background icon"
      , defaultValue = Nothing
      , setter = \widget -> \case
          Nothing -> Gtk.clearImageIconName =<< getImage widget
          Just v -> flip Gtk.setImageIconName v =<< getImage widget
      , getter = Gtk.getImageIconName <=< getImage
      , flags = Nothing
      }

  GI.gobjectInstallCIntProperty @ImageBar
    gClass
    GI.CIntPropertyInfo
      { name = T.pack "bar_icon_size"
      , nick = T.pack "Bar Icon Size"
      , blurb = T.pack "Size of the background icon"
      , defaultValue = 0
      , setter = \widget v -> flip Gtk.setImageIconSize (fromIntegral v) =<< getImage widget
      , getter = \widget -> fromIntegral <$> (Gtk.getImageIconSize =<< getImage widget)
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 6
      }

  -- Container should take care of "destroy"
  Just oldAllocate <- GI.get widgetClass #sizeAllocate

  GI.set widgetClass [#draw GI.:&= draw]
  GI.set
    widgetClass
    [ #getPreferredWidth GI.:&= prefSize #getPreferredWidth
    , #getPreferredHeight GI.:&= prefSize #getPreferredHeight
    , #getPreferredHeightForWidth GI.:&= prefRatio #getPreferredHeightForWidth
    , #getPreferredWidthForHeight GI.:&= prefRatio #getPreferredWidthForHeight
    , #sizeAllocate GI.:&= sizeAllocate oldAllocate
    ]

  -- Widget For CSS
  widgetClass.setCssName (T.pack "imagebar")
  where
    -- Somehow, style properties does not work...
    draw widget ctx = do
      mine <- GI.unsafeCastTo AsImageBar widget
      drawBar mine ctx
      pure True

    prefSize f widget = do
      image <- getImage =<< GI.unsafeCastTo AsImageBar widget
      f image
    prefRatio f widget other = do
      image <- getImage =<< GI.unsafeCastTo AsImageBar widget
      f image other

    sizeAllocate oldAllocate widget rect = do
      image <- getImage =<< GI.unsafeCastTo AsImageBar widget
      -- Allocate entire rectangle to the foreground image
      image.sizeAllocate rect
      oldAllocate widget rect

instanceInit :: GI.GObjectClass -> ImageBar -> IO Private
instanceInit _gclass inst = do
  #initTemplate inst
  barIcon <- Gtk.templateChild inst (T.pack "bar-foreground") Gtk.Image

  let barOrientation = Gtk.OrientationHorizontal
  let barScale = 1
  let barPortion = Rectangle 0 0 1 1
  barFill <- newIORef 0

  pure MkPrivate{..}

getImage :: ImageBar -> IO Gtk.Image
getImage imgBar = do
  MkPrivate{barIcon} <- GI.gobjectGetPrivateData imgBar
  pure barIcon

drawBar :: ImageBar -> Cairo.Context -> IO ()
drawBar widget ctxt = do
  MkPrivate{..} <- GI.gobjectGetPrivateData widget

  style <- widget.getStyleContext
  width <- fromIntegral <$> widget.getAllocatedWidth
  height <- fromIntegral <$> widget.getAllocatedHeight

  -- Background & Frame
  Gtk.renderBackground style ctxt 0 0 width height
  Gtk.renderFrame style ctxt 0 0 width height

  -- Use Foreground Color
  state <- style.getState
  Gdk.cairoSetSourceRgba ctxt =<< style.getColor state
  fill <- readIORef barFill
  -- Render bar rectangle
  (`Cairo.renderWithContext` ctxt) $ do
    let (fillX, fillY) = case barOrientation of
          Gtk.OrientationHorizontal -> (fill, 1.0)
          Gtk.OrientationVertical -> (1.0, fill)
          _ -> (1.0, 1.0)
    let Rectangle rx ry rw rh = barPortion
        rs = barScale
        ax = realToFrac rx * width / realToFrac rs
        ay = realToFrac ry * height / realToFrac rs
        aw = realToFrac rw * width / realToFrac rs
        ah = realToFrac rh * height / realToFrac rs
    Cairo.rectangle ax (ay + ah) (aw * fillX) (-ah * fillY)
    Cairo.fill

  -- Lastly, render the foreground image
  barIcon.draw ctxt

-- | Set Image bar position.
-- The portion is x, y, width, height.
setupPos :: ImageBar -> Gtk.Orientation -> Word -> Rectangle -> IO ()
setupPos widget orient scale portion = do
  GI.gobjectModifyPrivateData widget $ \dat ->
    dat
      { barOrientation = orient
      , barScale = scale
      , barPortion = portion
      }

setFill :: ImageBar -> Double -> IO ()
setFill widget fill = do
  MkPrivate{barFill} <- GI.gobjectGetPrivateData widget
  writeIORef barFill fill
  widget.queueDraw

setIcon :: ImageBar -> T.Text -> Gtk.IconSize -> IO ()
setIcon widget iconName iconSize = do
  image <- getImage widget
  Gtk.imageSetFromIconName image (Just iconName) (fromIntegral $ fromEnum iconSize)
