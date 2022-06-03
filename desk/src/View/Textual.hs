module View.Textual (
  LabelDyn,
  labelDynWidget,
  labelDynNew,
  labelDynSetLabel,
) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Gtk.Objects.Label qualified as UI
import UI.Commons qualified as UI

data LabelDyn = LabelDyn
  { labelDynWid :: !UI.Widget
  , labelDynLbl :: !UI.Label
  }

labelDynWidget :: LabelDyn -> UI.Widget
labelDynWidget LabelDyn{labelDynWid} = labelDynWid

labelDynNew :: MonadIO m => m LabelDyn
labelDynNew = do
  labelDynLbl <- UI.labelNew Nothing
  labelDynWid <- UI.toWidget labelDynLbl
  pure LabelDyn{..}

labelDynSetLabel :: MonadIO m => LabelDyn -> T.Text -> m ()
labelDynSetLabel LabelDyn{labelDynLbl} = UI.labelSetLabel labelDynLbl
