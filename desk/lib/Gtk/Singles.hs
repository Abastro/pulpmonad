module Gtk.Singles (
  module GI.Gtk.Objects.Label,
  imageNew,
) where

import Control.Concurrent.STM
import Control.Concurrent.Task
import Control.Monad.IO.Class
import Data.Text qualified as T
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector
import GI.Gtk.Objects.Image
import Gtk.Commons
import Gtk.Task
import GI.Gtk.Objects.Label
import XMonad.StackSet (RationalRect (..))
