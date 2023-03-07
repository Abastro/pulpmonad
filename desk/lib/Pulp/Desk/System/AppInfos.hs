{-# LANGUAGE OverloadedLabels #-}

-- | Caches app informations to obtain the list without IO overhead.
module Pulp.Desk.System.AppInfos (
  AppInfoData (..),
  appGetIns,
  AppInfoCol,
  trackAppInfo,
  getAppInfos,
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.GI.Base.ManagedPtr qualified as GI
import Data.GI.Base.Signals qualified as GI
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio qualified as Gio

data AppInfoData = AppInfoData
  { appId :: !T.Text
  , appExecName :: !(Maybe T.Text)
  -- ^ Executable name, which is first word of "Exec"
  , appWmClass :: !(Maybe T.Text)
  }

-- | Get the instance of desktop appinfo to query more information.
appGetIns :: AppInfoData -> IO (Maybe Gio.DesktopAppInfo)
appGetIns AppInfoData{appId} = Gio.desktopAppInfoNew appId

data AppInfoCol = AppInfoCol
  { curAppInfo :: !(TVar (Maybe (V.Vector AppInfoData)))
  -- ^ Current app info list, "Nothing" is invalidated state
  , appsMonitor :: !Gio.AppInfoMonitor
  }

trackAppInfo :: IO AppInfoCol
trackAppInfo = do
  curAppInfo <- newTVarIO Nothing
  appsMonitor <- Gio.appInfoMonitorGet
  GI.on appsMonitor #changed $ atomically $ writeTVar curAppInfo Nothing
  pure AppInfoCol{..}

getAppInfos :: AppInfoCol -> IO (V.Vector AppInfoData)
getAppInfos AppInfoCol{curAppInfo} =
  readTVarIO curAppInfo >>= maybe (getFromScratch >>= writeAndGet) pure
  where
    writeAndGet v = atomically (v <$ writeTVar curAppInfo (Just v))

    getFromScratch = do
      appInfos <- V.fromList <$> Gio.appInfoGetAll
      deskInfos <- V.mapMaybeM (GI.castTo Gio.DesktopAppInfo) appInfos
      V.mapM appInfoData deskInfos

    appInfoData (appInfo :: Gio.DesktopAppInfo) = do
      appId <- appInfo.getId
      appExecName <- getExecName appInfo
      appWmClass <- getWmClass appInfo
      pure AppInfoData{..}

    getExecName appInfo = runMaybeT $ do
      exec <- nullRetToMaybeT appInfo.getExecutable
      execName : _ <- pure (words exec)
      pure (T.pack execName)

    getWmClass appInfo = runMaybeT $ do
      nullRetToMaybeT appInfo.getStartupWmClass

    nullRetToMaybeT x = either (const empty) pure =<< liftIO (try @Gio.UnexpectedNullPointerReturn x)
