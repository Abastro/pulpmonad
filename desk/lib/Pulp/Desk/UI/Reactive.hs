{-# LANGUAGE GADTs #-}

module Pulp.Desk.UI.Reactive (
  uiSingleRun,
  uiCreate,
  onSource,
  gtkReact,
) where

import Control.Concurrent
import Control.Monad.Reader
import Data.GI.Base.BasicTypes qualified as GI
import Data.GI.Base.Signals qualified as GI
import GI.GLib.Constants qualified as GLib
import GI.Gdk.Functions qualified as Gdk
import Pulp.Desk.Reactive.Entry
import Pulp.Desk.UI.Commons
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

-- | Adds UI single-use task, which only runs once.
uiSingleRun :: IO a -> IO ()
uiSingleRun task = void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE (False <$ task)

-- | Creates using the action in UI thread and returns it through MVar.
--
-- Please do not put values into the MVar.
--
-- Should not be called from main thread in most cases.
uiCreate :: IO a -> IO (MVar a)
uiCreate make = do
  -- TODO Check if / Guard against main thread?
  res <- newEmptyMVar
  uiSingleRun $ make >>= putMVar res
  pure res

-- MAYBE Forks a new thread on every signal handling.

-- | Connect a signal and take it as a event source.
--
-- Connecting and disconnecting is both done in the UI thread.
--
-- Caveat: Can deadlock if Source is unregistered in UI thread while UI is being created.
onSource ::
  (GI.GObject obj, GI.SignalInfo info) =>
  obj ->
  GI.SignalProxy obj info ->
  (Handler a -> GI.HaskellCallbackType info) ->
  Source a
onSource obj proxy asCallback = sourceWithUnreg $ \handler -> do
  varHandlerId <- uiCreate $ GI.on obj proxy (asCallback handler)
  pure $ do
    handlerId <- takeMVar varHandlerId
    uiSingleRun $ GI.disconnectSignalHandler obj handlerId

gtkReact :: Event (BuilderM IO ()) -> BuilderM MomentIO ()
gtkReact evt = ReaderT $ \builder -> do
  reactimate $ uiSingleRun . (`runReaderT` builder) <$> evt
