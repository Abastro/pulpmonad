{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bar with image in the background.
module Gtk.ImageBar (
  ImageBar (..),
  imageBarPos,
  imageBarSetFill,
  imageBarSetIcon,
) where

import Data.Coerce
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
import GI.Gtk.Functions (renderBackground, renderFrame)
import GI.Gtk.Objects.Bin
import GI.Gtk.Objects.Image
import GI.Gtk.Structs.WidgetClass
import Graphics.X11.Xlib.Types (Rectangle (..))
import Gtk.Commons

-- TODO Figure out how to attach label for attributes

newtype ImageBar = ImageBar (ManagedPtr ImageBar)

instance TypedObject ImageBar where
  glibType = registerGType ImageBar
instance GObject ImageBar

-- MAYBE Add orientable? Sounds hard
type instance ParentTypes ImageBar = Bin ': ParentTypes Bin
instance HasParentTypes ImageBar

data ImageBarPrivate = ImageBarPrivate
  { barOrientation :: !Orientation
  , -- | Scale for the portion.
    barScale :: !Word
  , -- | Portion where bar is displayed. left - top - width - height.
    barPortion :: !Rectangle
  , barFill :: !(IORef Double)
  }

-- MAYBE Export portion to CSS?

instance DerivedGObject ImageBar where
  type GObjectParentType ImageBar = Bin
  type GObjectPrivateData ImageBar = ImageBarPrivate
  objectTypeName = T.pack "ImageBar"
  objectClassInit = imageBarClassInit
  objectInstanceInit = imageBarInstanceInit

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

withWidgetClass :: GObjectClass -> (WidgetClass -> IO b) -> IO b
withWidgetClass gclass act = withTransient (coerce gclass) $ act . WidgetClass

imageBarClassInit :: GObjectClass -> IO ()
imageBarClassInit gClass = withWidgetClass gClass $ \widgetClass -> do
  -- As GObject
  gobjectInstallCIntProperty @ImageBar
    gClass
    CIntPropertyInfo
      { -- Poor man's Orientable
        name = T.pack "orient"
      , nick = T.pack "Orientation"
      , blurb = T.pack "Orientation of the bar"
      , defaultValue = 0
      , setter = \widget v -> do
          gobjectModifyPrivateData widget $ \dat -> dat{barOrientation = (toEnum . fromIntegral) v}
      , getter = \widget -> do
          (fromIntegral . fromEnum) . barOrientation <$> gobjectGetPrivateData widget
      , flags = Nothing
      , minValue = Just 0
      , maxValue = Just 1
      }

  -- As Widget
  Just oldDestroy <- get widgetClass #destroy
  Just oldAllocate <- get widgetClass #sizeAllocate

  set widgetClass [#destroy :&= destroy oldDestroy]
  set widgetClass [#draw :&= draw]
  set
    widgetClass
    [ #getPreferredWidth :&= prefSize #getPreferredWidth
    , #getPreferredHeight :&= prefSize #getPreferredHeight
    , #getPreferredHeightForWidth :&= prefRatio #getPreferredHeightForWidth
    , #getPreferredWidthForHeight :&= prefRatio #getPreferredWidthForHeight
    , #sizeAllocate :&= sizeAllocate oldAllocate
    ]

  #setCssName widgetClass (T.pack "imagebar")
  where
    destroy oldDestroy widget = do
      Just mine <- castTo ImageBar widget
      image <- imageBarGetImage mine
      #destroy image -- The image needs to be destroyed
      oldDestroy widget

    draw widget ctx = do
      Just mine <- castTo ImageBar widget
      imageBarDraw mine ctx
      return True

    prefSize f widget = do
      Just mine <- castTo ImageBar widget
      image <- imageBarGetImage mine
      f image
    prefRatio f widget other = do
      Just mine <- castTo ImageBar widget
      image <- imageBarGetImage mine
      f image other

    sizeAllocate oldAllocate widget rect = do
      Just mine <- castTo ImageBar widget
      image <- imageBarGetImage mine
      -- Allocate entire rectangle to the foreground image
      #sizeAllocate image rect

      oldAllocate widget rect

imageBarInstanceInit :: GObjectClass -> ImageBar -> IO ImageBarPrivate
imageBarInstanceInit _gclass widget = do
  -- Window not needed (Takes no input)

  image <- new Image []
  #add widget image

  let barOrientation = OrientationHorizontal
  let barScale = 1
  let barPortion = Rectangle 0 0 1 1
  barFill <- newIORef 0

  pure ImageBarPrivate{..}

imageBarGetImage :: ImageBar -> IO Image
imageBarGetImage widget = do
  Just child <- binGetChild widget
  Just image <- castTo Image child
  pure image

imageBarDraw :: ImageBar -> C.Context -> IO ()
imageBarDraw widget ctxt = do
  ImageBarPrivate{..} <- gobjectGetPrivateData widget
  image <- imageBarGetImage widget

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
  #draw image ctxt

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
