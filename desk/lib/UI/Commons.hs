{-# LANGUAGE MonoLocalBinds #-}

module UI.Commons (
  module GI.Gtk.Enums,
  module GI.Gdk.Enums,
  module GI.Gtk.Constants,
  module GI.Gdk.Constants,
  module GI.Gtk.Objects.Widget,
) where

import GI.Gdk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gdk.Enums hiding (AnotherWindowType, WindowType, WindowTypeToplevel)
import GI.Gtk.Constants hiding (MAJOR_VERSION, MICRO_VERSION, MINOR_VERSION)
import GI.Gtk.Enums
import GI.Gtk.Objects.Widget

-- TODO
-- Taffybar is suboptimal, esp. dependency. Cook one up myself?
