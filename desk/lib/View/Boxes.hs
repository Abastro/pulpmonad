{-# LANGUAGE OverloadedLabels  #-}

module View.Boxes (
  BoxArg (..),
  BoxPack (..),
  BoxUniDyn,
  BoxUniDynOp (..),
  defBoxArg,
  boxUniDynWidget,
  boxUniDynNew,
  boxUniDynCtrl,
  boxStaticNew,
) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import GI.Gtk.Objects.Box qualified as Gtk
import Gtk.Commons qualified as Gtk
import Data.GI.Base.Constructible
import Data.GI.Base.Attributes

-- TODO Instead, offload UI to XML - how?

data BoxArg = BoxArg
  { boxOrient :: !Gtk.Orientation
  , boxPacking :: !BoxPack
  , boxSpacing :: !Int32
  , boxHomogeneous :: !Bool
  }

data BoxPack = BoxPackDef | BoxPackStart | BoxPackEnd

defBoxArg :: Gtk.Orientation -> BoxArg
defBoxArg orient =
  BoxArg
    { boxOrient = orient
    , boxPacking = BoxPackDef
    , boxSpacing = 0
    , boxHomogeneous = False
    }

-- | Unidirectional box.
data BoxUniDyn = BoxUniDyn
  { boxDynWid :: !Gtk.Widget
  , boxDynBox :: !Gtk.Box
  , boxDynPack :: !BoxPack
  }

data BoxUniDynOp
  = -- | Adds widget to the box. Does not realize the widget.
    BoxUniAdd !Gtk.Widget
  | -- | Removes widget from the box. Does not unrealize/delete the widget.
    BoxUniRemove !Gtk.Widget
  | -- | Reorders widget to certain index.
    BoxUniReorder !Gtk.Widget !Int32

boxUniDynWidget :: BoxUniDyn -> Gtk.Widget
boxUniDynWidget BoxUniDyn{boxDynWid} = boxDynWid

boxUniDynNew :: MonadIO m => BoxArg -> m BoxUniDyn
boxUniDynNew BoxArg{..} = do
  boxDynBox <- new Gtk.Box [#orientation := boxOrient, #spacing := boxSpacing, #homogeneous := boxHomogeneous]
  boxDynWid <- Gtk.toWidget boxDynBox
  pure BoxUniDyn{boxDynPack = boxPacking, ..}

boxUniDynCtrl :: MonadIO m => BoxUniDyn -> BoxUniDynOp -> m ()
boxUniDynCtrl BoxUniDyn{..} = \case
  BoxUniAdd wid -> boxPack boxDynBox wid
  BoxUniRemove wid -> #remove boxDynBox wid
  BoxUniReorder wid idx -> #reorderChild boxDynBox wid idx
  where
    boxPack (box :: Gtk.Box) wid = case boxDynPack of
      BoxPackDef -> #add box wid
      BoxPackStart -> #packStart box wid False False 0
      BoxPackEnd -> #packEnd box wid False False 0

-- | Static box with fixed children.
boxStaticNew :: MonadIO m => BoxArg -> [Gtk.Widget] -> m Gtk.Widget
boxStaticNew arg children = do
  dyn <- boxUniDynNew arg
  traverse_ (boxUniDynCtrl dyn . BoxUniAdd) children
  pure $ boxUniDynWidget dyn
