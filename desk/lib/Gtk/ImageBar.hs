{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bar with image in the background.
module Gtk.ImageBar (
  ImageBar (..),
  imageBarPos,
  imageBarSetFill,
) where

import Data.Coerce
import Data.GI.Base
import Data.GI.Base.GObject
import Data.GI.Base.Overloading
import Data.IORef
import Data.Text qualified as T
import GHC.OverloadedLabels (IsLabel (..))
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as C
import GI.Cairo.Structs.Context qualified as C
import GI.Gdk.Functions qualified as Gdk
import GI.Gtk.Functions (renderBackground, renderFrame)
import GI.Gtk.Objects.Image
import GI.Gtk.Structs.WidgetClass
import Gtk.Commons
import XMonad.StackSet (RationalRect (..))

-- ImageBar is a final class, so no class dedicated for ImageBar
newtype ImageBar = ImageBar (ManagedPtr ImageBar)

instance TypedObject ImageBar where
  glibType = registerGType ImageBar
instance GObject ImageBar

-- MAYBE Add orientable? Sounds hard
type instance ParentTypes ImageBar = Widget ': ParentTypes Widget
instance HasParentTypes ImageBar

data ImageBarPrivate = ImageBarPrivate
  { foregroundImage :: !Image
  , barOrientation :: !Orientation
  , -- | Portion where bar is displayed. left - top - width - height.
    barPortion :: !RationalRect
  , barFill :: !(IORef Double)
  }

-- MAYBE Export portion to CSS?

instance DerivedGObject ImageBar where
  type GObjectParentType ImageBar = Widget
  type GObjectPrivateData ImageBar = ImageBarPrivate
  objectTypeName = T.pack "image-bar"
  objectClassInit = imageBarClassInit
  objectInstanceInit = imageBarInstanceInit

type instance AttributeList ImageBar = AttributeList Widget
instance HasAttributeList ImageBar
type instance SignalList ImageBar = SignalList Widget
instance
  ( info ~ ResolveWidgetMethod t ImageBar
  , OverloadedMethod info ImageBar p
  ) =>
  IsLabel t (ImageBar -> p)
  where
  fromLabel = overloadedMethod @info

withWidgetClass :: GObjectClass -> (WidgetClass -> IO b) -> IO b
withWidgetClass gclass act = withTransient (coerce gclass) $ act . WidgetClass

imageBarClassInit :: GObjectClass -> IO ()
imageBarClassInit = flip withWidgetClass $ \widgetClass -> do
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
  where
    destroy oldDestroy widget = do
      Just mine <- castTo ImageBar widget
      ImageBarPrivate{foregroundImage} <- gobjectGetPrivateData mine
      #destroy foregroundImage -- The image needs to be destroyed
      oldDestroy widget

    draw widget ctx = do
      Just mine <- castTo ImageBar widget
      imageBarDraw mine ctx
      return True

    prefSize f widget = do
      Just mine <- castTo ImageBar widget
      ImageBarPrivate{foregroundImage} <- gobjectGetPrivateData mine
      f foregroundImage
    prefRatio f widget other = do
      Just mine <- castTo ImageBar widget
      ImageBarPrivate{foregroundImage} <- gobjectGetPrivateData mine
      f foregroundImage other

    sizeAllocate oldAllocate widget rect = do
      Just mine <- castTo ImageBar widget
      ImageBarPrivate{foregroundImage} <- gobjectGetPrivateData mine
      -- Allocate entire rectangle to the foreground image
      #sizeAllocate foregroundImage rect

      oldAllocate widget rect

imageBarInstanceInit :: GObjectClass -> ImageBar -> IO ImageBarPrivate
imageBarInstanceInit _gclass _widget = do
  -- Window not needed: Takes no input.

  foregroundImage <- new Image []
  let barOrientation = OrientationHorizontal
  let barPortion = RationalRect 0 0 1 1
  barFill <- newIORef 0

  pure ImageBarPrivate{..}

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
    let RationalRect rx ry rw rh = barPortion
        ax = realToFrac rx * realToFrac width
        ay = realToFrac ry * realToFrac height
        aw = realToFrac rw * realToFrac width
        ah = realToFrac rh * realToFrac height
    C.rectangle ax (ay + ah) (aw * fillX) (-ah * fillY)
    C.fill

  -- Lastly, render the foreground image
  #draw foregroundImage ctxt

-- TODO Image Icon Set
-- TODO CSS Handling

-- I do not know how to properly set up labels :/

imageBarPos :: ImageBar -> Orientation -> RationalRect -> IO ()
imageBarPos widget orient portion = do
  gobjectModifyPrivateData widget $ \dat -> dat{ barOrientation = orient, barPortion = portion }

imageBarSetFill :: ImageBar -> Double -> IO ()
imageBarSetFill widget fill = do
  ImageBarPrivate{barFill} <- gobjectGetPrivateData widget
  writeIORef barFill fill
