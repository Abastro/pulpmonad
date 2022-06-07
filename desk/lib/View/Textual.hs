module View.Textual (
  LabelDyn,
  LabelDynArgs (..),
  defLabelDyn,
  labelDynWidget,
  labelDynNew,
  labelDynSetLabel,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as Gtk
import Gtk.Commons qualified as Gtk

data LabelDynArgs = LabelDynArgs
  { labelLineWrap :: !Bool
  , labelJustify :: !Gtk.Justification
  }

defLabelDyn :: LabelDynArgs
defLabelDyn =
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
labelDynNew :: MonadIO m => LabelDynArgs -> m LabelDyn
labelDynNew LabelDynArgs{..} = do
  labelDynLbl <- Gtk.labelNew Nothing
  labelDynWid <- Gtk.toWidget labelDynLbl
  Gtk.labelSetLineWrap labelDynLbl labelLineWrap
  Gtk.labelSetJustify labelDynLbl labelJustify
  pure LabelDyn{..}

labelDynSetLabel :: MonadIO m => LabelDyn -> T.Text -> m ()
labelDynSetLabel LabelDyn{labelDynLbl} = Gtk.labelSetLabel labelDynLbl
