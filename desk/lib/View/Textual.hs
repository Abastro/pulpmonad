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
import GI.Gtk.Objects.Label qualified as UI
import UI.Commons qualified as UI

data LabelDynArgs = LabelDynArgs
  { labelLineWrap :: !Bool
  , labelJustify :: !UI.Justification
  }

defLabelDyn :: LabelDynArgs
defLabelDyn =
  LabelDynArgs
    { labelLineWrap = True
    , labelJustify = UI.JustificationLeft
    }

data LabelDyn = LabelDyn
  { labelDynWid :: !UI.Widget
  , labelDynLbl :: !UI.Label
  }

labelDynWidget :: LabelDyn -> UI.Widget
labelDynWidget LabelDyn{labelDynWid} = labelDynWid

-- | Construct dynamic label. When the flag passed is true, label is line-wrapped.
labelDynNew :: MonadIO m => LabelDynArgs -> m LabelDyn
labelDynNew LabelDynArgs{..} = do
  labelDynLbl <- UI.labelNew Nothing
  labelDynWid <- UI.toWidget labelDynLbl
  UI.labelSetLineWrap labelDynLbl labelLineWrap
  UI.labelSetJustify labelDynLbl labelJustify
  pure LabelDyn{..}

labelDynSetLabel :: MonadIO m => LabelDyn -> T.Text -> m ()
labelDynSetLabel LabelDyn{labelDynLbl} = UI.labelSetLabel labelDynLbl
