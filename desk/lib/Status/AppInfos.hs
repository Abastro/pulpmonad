-- | Caches app informations to obtain the list without IO overhead.
module Status.AppInfos (
  AppInfoData (..),
  appGetIns,
  AppInfoCol,
  trackAppInfo,
  getAppInfos,
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Trans.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Gio qualified as Gio
import Control.Monad.IO.Class
import Control.Exception

data AppInfoData = AppInfoData
  { appId :: !T.Text
  , -- | Executable name, which is first word of "Exec"
    appExecName :: !(Maybe T.Text)
  , appWmClass :: !(Maybe T.Text)
  }

-- | Get the instance of desktop appinfo to query more information.
appGetIns :: AppInfoData -> IO (Maybe Gio.DesktopAppInfo)
appGetIns AppInfoData{appId} = Gio.desktopAppInfoNew appId

data AppInfoCol = AppInfoCol
  { -- | Current app info list, "Nothing" is invalidated state
    curAppInfo :: !(TVar (Maybe (V.Vector AppInfoData)))
  , appsMonitor :: !Gio.AppInfoMonitor
  }

trackAppInfo :: IO AppInfoCol
trackAppInfo = do
  curAppInfo <- newTVarIO Nothing
  appsMonitor <- Gio.appInfoMonitorGet
  Gio.onAppInfoMonitorChanged appsMonitor $ atomically $ writeTVar curAppInfo Nothing
  pure AppInfoCol{..}

getAppInfos :: AppInfoCol -> IO (V.Vector AppInfoData)
getAppInfos AppInfoCol{curAppInfo} =
  readTVarIO curAppInfo >>= maybe (getFromScratch >>= writeAndGet) pure
  where
    writeAndGet v = atomically (v <$ writeTVar curAppInfo (Just v))

    getFromScratch = do
      appInfos <- V.fromList <$> Gio.appInfoGetAll
      deskInfos <- V.mapMaybeM (Gio.castTo Gio.DesktopAppInfo) appInfos
      V.mapM appInfoData deskInfos

    appInfoData appInfo = do
      Just appId <- Gio.appInfoGetId appInfo -- Expect App ID to exist.
      appExecName <- getExecName appInfo
      appWmClass <- getWmClass appInfo
      pure AppInfoData{..}

    getExecName appInfo = runMaybeT $ do
      exec <- either (const empty) pure =<< liftIO (tryNull $ Gio.appInfoGetExecutable appInfo)
      execName : _ <- pure (words exec)
      pure (T.pack execName)

    getWmClass = Gio.desktopAppInfoGetStartupWmClass

    tryNull = try @Gio.UnexpectedNullPointerReturn
