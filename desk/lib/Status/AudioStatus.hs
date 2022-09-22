-- | Audio status from ALSA library. Derived from xmobar's volume module.
module Status.AudioStatus (
  VolStat (..),
  curVolStat,
  setRelVolume,
  setUnmuted,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Sound.ALSA.Exception qualified as Alsa
import Sound.ALSA.Mixer

-- TODO Fix sound indicator occasionally going mute/0 until sound play

catchWithDef def act = Alsa.catch act $ const (pure def)

-- | Switch/volume capability could be either
-- 1. Common to playback and capture
-- 2. Separate capability for each
--
-- Playback is outbound sound (e.g. playing), Capture is inbound sound (e.g. recording).
playCapa caps = playback caps <|> common caps

toRatio lo hi cur = fromIntegral (cur - lo) / fromIntegral (hi - lo)
fromRatio lo hi per = lo + floor (fromIntegral (hi - lo) * per)

data VolStat = VolStat
  { curVolume :: !Double
  -- ^ Current volume in ratio
  , isUnmuted :: !Bool
  -- ^ Whether the flag is unmuted or muted, False = muted
  }
  deriving (Show)

withMixerCtrl :: String -> String -> (Control -> MaybeT IO a) -> IO (Maybe a)
withMixerCtrl mixerName controlName act = withMixer mixerName $ \mixer -> runMaybeT $ do
  control <- MaybeT $ getControlByName mixer controlName
  act control

-- | Current playback volume status.
-- First arg is mixer name (e.g. "default"), second arg is control name (e.g. "Master").
curVolStat :: String -> String -> IO (Maybe VolStat)
curVolStat mixerName controlName =
  catchWithDef Nothing . withMixerCtrl mixerName controlName $ \control -> do
    curVol <- getVolume control
    curSwit <- liftIO . runMaybeT $ getSwitch control -- Optional
    pure VolStat{curVolume = curVol, isUnmuted = fromMaybe False curSwit}
  where
    getVolume control = do
      volCapa <- MaybeT . pure $ playCapa (volume control)
      (lo, hi) <- liftIO $ getRange volCapa
      val <- defChannelV (value volCapa)
      pure $ toRatio lo hi val

    getSwitch control = do
      switCapa <- MaybeT . pure $ playCapa (switch control)
      defChannelV switCapa

    -- We assume FrontLeft would be enough.
    defChannelV perCh = MaybeT $ catchWithDef Nothing (getChannel FrontLeft perCh)

-- | Sets playback volume in percentage.
setRelVolume :: String -> String -> Double -> IO ()
setRelVolume mixerName controlName newVol = do
  catchWithDef Nothing . withMixerCtrl mixerName controlName $ \control -> do
    volCapa <- MaybeT . pure $ playCapa (volume control)
    (lo, hi) <- liftIO $ getRange volCapa
    liftIO . catchWithDef () $ setChannel FrontLeft (value volCapa) (fromRatio lo hi newVol)
  pure ()

-- | Sets unmuted flag (False = muted).
setUnmuted :: String -> String -> Bool -> IO ()
setUnmuted mixerName controlName newSwit = do
  catchWithDef Nothing . withMixerCtrl mixerName controlName $ \control -> do
    switCapa <- MaybeT . pure $ playCapa (switch control)
    liftIO . catchWithDef () $ setChannel FrontLeft switCapa newSwit
  pure ()
