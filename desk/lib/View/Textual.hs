module View.Textual (
  LabelDyn,
  LabelArgs (..),
  defLabelArg,
  labelDynWidget,
  labelDynNew,
  labelDynSetLabel,
  labelStaticNew,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk

data LabelArgs = LabelDynArgs
  { labelLineWrap :: !Bool
  , labelJustify :: !Gtk.Justification
  }

defLabelArg :: LabelArgs
defLabelArg =
  LabelDynArgs
    { labelLineWrap = True
    , labelJustify = Gtk.JustificationLeft
    }

data LabelDyn = LabelDyn
  { labelDynWid :: !Gtk.Widget
  , labelDynLbl :: !Gtk.Label
  }

labelDynWidget :: LabelDyn -> Gtk.Widget
labelDynWidget LabelDyn{labelDynWid} = labelDynWid

-- | Construct dynamic label. When the flag passed is true, label is line-wrapped.
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
