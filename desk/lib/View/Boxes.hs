module View.Boxes (
  BoxArg (..),
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
import Gtk.Containers qualified as Gtk

data BoxArg = BoxArg
  { boxOrient :: !Gtk.Orientation
  , boxSpacing :: !Int32
  , boxHomogeneous :: !Bool
  }

defBoxArg :: Gtk.Orientation -> BoxArg
defBoxArg orient =
  BoxArg
    { boxOrient = orient
    , boxSpacing = 0
    , boxHomogeneous = False
    }

-- | Unidirectional box, from left to right
data BoxUniDyn = BoxUniDyn
  { boxDynWid :: !Gtk.Widget
  , boxDynBox :: !Gtk.Box
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
  boxDynBox <- Gtk.boxNew boxOrient boxSpacing
  Gtk.boxSetHomogeneous boxDynBox boxHomogeneous
  boxDynWid <- Gtk.toWidget boxDynBox
  pure BoxUniDyn{..}

boxUniDynCtrl :: MonadIO m => BoxUniDyn -> BoxUniDynOp -> m ()
boxUniDynCtrl BoxUniDyn{boxDynBox} = \case
  BoxUniAdd wid -> Gtk.containerAdd boxDynBox wid
  BoxUniRemove wid -> Gtk.containerRemove boxDynBox wid
  BoxUniReorder wid idx -> Gtk.boxReorderChild boxDynBox wid idx

-- | Static box with fixed children.
boxStaticNew :: MonadIO m => BoxArg -> [Gtk.Widget] -> m Gtk.Widget
boxStaticNew arg children = do
  dyn <- boxUniDynNew arg
  traverse_ (boxUniDynCtrl dyn . BoxUniAdd) children
  pure $ boxUniDynWidget dyn
