module Lib (
    startApp,
) where

import Control.Monad.Logger (
    runNoLoggingT,
    runStdoutLoggingT,
 )
import Env (
    Env (
        Env,
        envLogFunc
    ),
 )
import Network.Wai.Handler.Warp (run)
import RIO
import Server (buildApp)
import System.IO (BufferMode (NoBuffering))

startApp :: IO ()
startApp = do
    hSetBuffering stdout NoBuffering
    logOpts <- logOptionsHandle stdout verboseLog
    withLogFunc logOpts $ \logFunc -> do
        runNoLoggingT $ do
            let env = Env{envLogFunc = logFunc}
            runRIO env $ do
                logInfo $ "[Info] listening on port " <> displayShow port <> " ..."
                liftIO $ run port (buildApp env)
  where
    verboseLog = False
    port = 8080
