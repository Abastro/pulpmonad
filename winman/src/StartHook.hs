module StartHook where

import Control.Monad
import Data.Coerce
import Data.Kind (Type)
import Data.Proxy
import Data.Set qualified as S
import Data.Typeable
import XMonad
import XMonad.Util.ExtensibleState qualified as XS

modifyingM :: (ExtensionClass a) => (a -> X a) -> X ()
modifyingM f = XS.get >>= f >>= XS.put

newtype PerformOnce (t :: Type) = PerformOnce (S.Set String)
  deriving (Read, Show)

instance (Typeable t) => ExtensionClass (PerformOnce t) where
  initialValue = PerformOnce mempty
  extensionType = PersistentExtension

-- | Perform an action only once, alike `spawnOnce`. The proxy type should be unique.
performOnce :: forall (t :: Type). Typeable t => Proxy t -> X () -> X ()
performOnce proxy run =
  modifyingM @(PerformOnce t) $ fmap coerce . S.alterF noting desig . coerce
  where
    desig = show (typeOf proxy)
    noting b = True <$ unless b run

-- | Designate initiation.
data Initiate

-- What would be here?
