module Pulp.Desk.Reactive.Task (
  Task (..),
) where

-- | Represents a repeatedly run Task.
-- Emits result each time task is run, allowing other threads to inspect for results.
--
-- The result could be inspected through 'emit', which could block and wait.
--
-- Can be killed using 'stop'.
data Task a = MkTask {stop :: IO (), emit :: IO a}
