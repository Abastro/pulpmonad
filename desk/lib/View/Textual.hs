module View.Textual (
  LabelDyn,
  LabelArgs (..),
  labelDynWidget,
  labelDynNew,
  labelDynSetLabel,
  labelStaticNew,
) where

import Control.Monad.IO.Class
import Data.Default.Class
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk

data LabelArgs = LabelDynArgs
  { labelLineWrap :: !Bool
  , labelJustify :: !Gtk.Justification
  }

instance Default LabelArgs where
  def =
    LabelDynArgs
      { labelLineWrap = False
      , labelJustify = Gtk.JustificationLeft
      }

data LabelDyn = LabelDyn
  { labelDynWid :: !Gtk.Widget
  , labelDynLbl :: !Gtk.Label
  }

labelDynWidget :: LabelDyn -> Gtk.Widget
labelDynWidget LabelDyn{labelDynWid} = labelDynWid

-- | Construct dynamic label.
labelDynNew :: MonadIO m => LabelArgs -> m LabelDyn
labelDynNew LabelDynArgs{..} = do
  labelDynLbl <- Gtk.labelNew Nothing
  labelDynWid <- Gtk.toWidget labelDynLbl
  Gtk.labelSetLineWrap labelDynLbl labelLineWrap
  Gtk.labelSetJustify labelDynLbl labelJustify
  pure LabelDyn{..}

labelDynSetLabel :: MonadIO m => LabelDyn -> T.Text -> m ()
labelDynSetLabel LabelDyn{labelDynLbl} = Gtk.labelSetLabel labelDynLbl

-- | Construct static label.
labelStaticNew :: MonadIO m => LabelArgs -> T.Text -> m Gtk.Widget
labelStaticNew args lbl = do
  dyn <- labelDynNew args
  labelDynSetLabel dyn lbl
  pure $ labelDynWidget dyn
