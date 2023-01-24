{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bar with image in the background.
-- Exists since Gtk does not allow scaling by proportion,
-- which is required to place bar at specific part of the icon.
module Gtk.ImageBar (
  ImageBar (..),
  imageBarPos,
  imageBarSetFill,
  imageBarSetIcon,
) where

import Control.Monad
import Data.GI.Base
import Data.GI.Base.GObject
import Data.GI.Base.GParamSpec
import Data.GI.Base.Overloading
import Data.IORef
import Data.Text qualified as T
import GHC.OverloadedLabels (IsLabel (..))
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as C
import GI.Cairo.Structs.Context qualified as C
import GI.Gdk.Functions qualified as Gdk
import GI.Gtk.Objects.Bin
import GI.Gtk.Objects.Image
import GI.Gtk.Structs.WidgetClass
import Graphics.X11.Xlib.Types (Rectangle (..))
import Gtk.Commons
import qualified GI.Gio.Interfaces.File as Gio
import System.Pulp.PulpPath
import qualified GI.Gio.Objects.Cancellable as Gio

-- MAYBE Figure out how to attach label for attributes. This is not crucial, though.

newtype ImageBar = ImageBar (ManagedPtr ImageBar)

instance TypedObject ImageBar where
  glibType :: IO GType
  glibType = registerGType ImageBar
instance GObject ImageBar

-- MAYBE Add orientable? Sounds hard
type instance ParentTypes ImageBar = Bin ': ParentTypes Bin
instance HasParentTypes ImageBar

data ImageBarPrivate = ImageBarPrivate
  { barOrientation :: !Orientation
  , barScale :: !Word
  -- ^ Scale for the portion.
  , barPortion :: !Rectangle
  -- ^ Portion where bar is displayed. left - top - width - height.
  , barFill :: !(IORef Double)
  , barIcon :: !Image
  }

-- MAYBE Export portion to CSS?

type instance AttributeList ImageBar = AttributeList Bin
instance HasAttributeList ImageBar
type instance SignalList ImageBar = SignalList Bin
instance
  ( info ~ ResolveBinMethod t ImageBar
  , OverloadedMethod info ImageBar p
  ) =>
  IsLabel t (ImageBar -> p)
  where
  fromLabel = overloadedMethod @info

instance DerivedGObject ImageBar where
  type GObjectParentType ImageBar = Bin
  type GObjectPrivateData ImageBar = ImageBarPrivate

  objectTypeName :: T.Text
  objectTypeName = T.pack "ImageBar"

  objectClassInit :: GObjectClass -> IO ()
  objectClassInit = imageBarClassInit

  objectInstanceInit :: GObjectClass -> ImageBar -> IO (GObjectPrivateData ImageBar)
  objectInstanceInit = imageBarInstanceInit

imageBarClassInit :: GObjectClass -> IO ()
imageBarClassInit gClass = withClassAs WidgetClass gClass $ \widgetClass -> do
  uiFile <- Gio.fileNewForPath =<< dataPath ("ui" </> "image-bar.ui")
  (bytes, _) <- #loadBytes uiFile (Nothing @Gio.Cancellable)
  #setTemplate widgetClass bytes
  #bindTemplateChildFull widgetClass (T.pack "bar-foreground") True 0

  -- As GObject
  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { -- Poor man's Orientable
        name = T.pack "orient"
      , nick = T.pack "Orientation"
      , blurb = T.pack "Orientation of the bar"
      , defaultValue = 0
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barOrientation = (toEnum . fromIntegral) v}
      , getter = \widget -> do
          (fromIntegral . fromEnum) . barOrientation <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 1
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_scale"
      , nick = T.pack "Bar Scale"
      , blurb = T.pack "Scale for the portion specification"
      , defaultValue = 1
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barScale = fromIntegral v}
      , getter = \widget -> do
          fromIntegral . barScale <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 1
      , maxValue = Just 256
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_portion_left"
      , nick = T.pack "Portion Left"
      , blurb = T.pack "Left spacing of the portion"
      , defaultValue = 0
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = (barPortion dat){rect_x = fromIntegral v}}
      , getter = \widget -> do
          fromIntegral . rect_x . barPortion <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_portion_top"
      , nick = T.pack "Portion Top"
      , blurb = T.pack "Top spacing of the portion"
      , defaultValue = 0
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = (barPortion dat){rect_y = fromIntegral v}}
      , getter = \widget -> do
          fromIntegral . rect_y . barPortion <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_portion_width"
      , nick = T.pack "Portion Width"
      , blurb = T.pack "Width of the portion"
      , defaultValue = 0
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = (barPortion dat){rect_width = fromIntegral v}}
      , getter = \widget -> do
          fromIntegral . rect_width . barPortion <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_portion_height"
      , nick = T.pack "Portion Height"
      , blurb = T.pack "Height of the portion"
      , defaultValue = 0
      , setter = \widget v -> gobjectModifyPrivateData widget $
          \dat -> dat{barPortion = (barPortion dat){rect_height = fromIntegral v}}
      , getter = \widget -> do
          fromIntegral . rect_height . barPortion <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 256
      }

  gobjectInstallCStringProperty @ImageBar
    gClass
    CStringPropertyInfo
      { name = T.pack "bar_icon_name"
      , nick = T.pack "Bar Icon Name"
      , blurb = T.pack "Name of the background icon"
      , defaultValue = Nothing
      , setter = \widget -> \case
          Nothing -> clearImageIconName =<< imageBarGetImage widget
          Just v -> flip setImageIconName v =<< imageBarGetImage widget
      , getter = getImageIconName <=< imageBarGetImage
      , flags = Nothing
      }

  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { name = T.pack "bar_icon_size"
      , nick = T.pack "Bar Icon Size"
      , blurb = T.pack "Size of the background icon"
      , defaultValue = 0
      , setter = \widget v -> flip setImageIconSize (fromIntegral v) =<< imageBarGetImage widget
      , getter = \widget -> fromIntegral <$> (getImageIconSize =<< imageBarGetImage widget)
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 6
      }

  -- Container should take care of "destroy"
  Just oldAllocate <- get widgetClass #sizeAllocate

  set widgetClass [#draw :&= draw]
  set
    widgetClass
    [ #getPreferredWidth :&= prefSize #getPreferredWidth
    , #getPreferredHeight :&= prefSize #getPreferredHeight
    , #getPreferredHeightForWidth :&= prefRatio #getPreferredHeightForWidth
    , #getPreferredWidthForHeight :&= prefRatio #getPreferredWidthForHeight
    , #sizeAllocate :&= sizeAllocate oldAllocate
    ]

  -- Widget For CSS
  #setCssName widgetClass (T.pack "imagebar")
  where
    -- Somehow, style properties does not work...
    draw widget ctx = do
      mine <- unsafeCastTo ImageBar widget
      imageBarDraw mine ctx
      pure True

    prefSize f widget = do
      image <- imageBarGetImage =<< unsafeCastTo ImageBar widget
      f image
    prefRatio f widget other = do
      image <- imageBarGetImage =<< unsafeCastTo ImageBar widget
      f image other

    sizeAllocate oldAllocate widget rect = do
      image <- imageBarGetImage =<< unsafeCastTo ImageBar widget
      -- Allocate entire rectangle to the foreground image
      #sizeAllocate image rect
      oldAllocate widget rect

imageBarInstanceInit :: GObjectClass -> ImageBar -> IO ImageBarPrivate
imageBarInstanceInit _gclass inst = do
  #initTemplate inst
  barIcon <- templateChild inst (T.pack "bar-foreground") Image

  let barOrientation = OrientationHorizontal
  let barScale = 1
  let barPortion = Rectangle 0 0 1 1
  barFill <- newIORef 0

  pure ImageBarPrivate{..}

imageBarGetImage :: ImageBar -> IO Image
imageBarGetImage imgBar = do
  ImageBarPrivate{barIcon} <- gobjectGetPrivateData imgBar
  pure barIcon

imageBarDraw :: ImageBar -> C.Context -> IO ()
imageBarDraw widget ctxt = do
  ImageBarPrivate{..} <- gobjectGetPrivateData widget

  style <- #getStyleContext widget
  width <- fromIntegral <$> #getAllocatedWidth widget
  height <- fromIntegral <$> #getAllocatedHeight widget

  -- Background & Frame
  renderBackground style ctxt 0 0 width height
  renderFrame style ctxt 0 0 width height

  -- Use Foreground Color
  state <- #getState style
  Gdk.cairoSetSourceRgba ctxt =<< #getColor style state
  fill <- readIORef barFill
  -- Render bar rectangle
  (`C.renderWithContext` ctxt) $ do
    let (fillX, fillY) = case barOrientation of
          OrientationHorizontal -> (fill, 1.0)
          OrientationVertical -> (1.0, fill)
          _ -> (1.0, 1.0)
    let Rectangle rx ry rw rh = barPortion
        rs = barScale
        ax = realToFrac rx * width / realToFrac rs
        ay = realToFrac ry * height / realToFrac rs
        aw = realToFrac rw * width / realToFrac rs
        ah = realToFrac rh * height / realToFrac rs
    C.rectangle ax (ay + ah) (aw * fillX) (-ah * fillY)
    C.fill

  -- Lastly, render the foreground image
  #draw barIcon ctxt

-- I do not know how to properly set up labels :/

-- | Set Image bar position.
-- The portion is x, y, width, height.
imageBarPos :: ImageBar -> Orientation -> Word -> Rectangle -> IO ()
imageBarPos widget orient scale portion = do
  gobjectModifyPrivateData widget $ \dat ->
    dat
      { barOrientation = orient
      , barScale = scale
      , barPortion = portion
      }

imageBarSetFill :: ImageBar -> Double -> IO ()
imageBarSetFill widget fill = do
  ImageBarPrivate{barFill} <- gobjectGetPrivateData widget
  writeIORef barFill fill
  #queueDraw widget

imageBarSetIcon :: ImageBar -> T.Text -> IconSize -> IO ()
imageBarSetIcon widget iconName iconSize = do
  image <- imageBarGetImage widget
  imageSetFromIconName image (Just iconName) (fromIntegral $ fromEnum iconSize)
