-- | Audio status from ALSA library. Derived from xmobar's volume module.
module Status.AudioStatus (
  VolStat (..),
  curVolStat,
  updateRelVolume,
  updateUnmuted,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Maybe
import Sound.ALSA.Exception qualified as Alsa
import Sound.ALSA.Mixer

catchWithDef def act = Alsa.catch act $ const (pure def)

-- | Switch/volume capability could be either
-- 1. Common to playback and capture
-- 2. Separate capability for each
--
-- Playback is outbound sound (e.g. playing), Capture is inbound sound (e.g. recording).
playCapa caps = playback caps <|> common caps

toRatio lo hi cur = fromIntegral (cur - lo) / fromIntegral (hi - lo)
fromRatio lo hi per = lo + floor (fromIntegral (hi - lo) * per)

setAllChannel perCh raw = traverse_ (\ch -> setChannel ch perCh raw) (channels perCh)

data VolStat = VolStat
  { curVolume :: !Rational
  -- ^ Current volume in ratio
  , isUnmuted :: !Bool
  -- ^ Whether the flag is unmuted or muted, False = muted
  }
  deriving (Show, Eq)

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

-- | Updates playback volume in percentage.
updateRelVolume :: String -> String -> (Rational -> Rational) -> IO ()
updateRelVolume mixerName controlName volUpd = do
  catchWithDef Nothing . withMixerCtrl mixerName controlName $ \control -> do
    volCapa <- MaybeT . pure $ playCapa (volume control)
    (lo, hi) <- liftIO $ getRange volCapa
    let vol = value volCapa
    curRaw <- MaybeT $ catchWithDef Nothing $ getChannel FrontLeft vol
    let newRaw = updated lo hi curRaw
    liftIO . catchWithDef () $ setAllChannel vol newRaw
  pure ()
  where
    updated lo hi = fromRatio lo hi . volUpd . toRatio lo hi

-- | Updates unmuted flag (False = muted).
updateUnmuted :: String -> String -> (Bool -> Bool) -> IO ()
updateUnmuted mixerName controlName switUpd = do
  catchWithDef Nothing . withMixerCtrl mixerName controlName $ \control -> do
    switCapa <- MaybeT . pure $ playCapa (switch control)
    curSwit <- MaybeT $ catchWithDef Nothing $ getChannel FrontLeft switCapa
    liftIO . catchWithDef () $ setAllChannel switCapa (switUpd curSwit)
  pure ()
