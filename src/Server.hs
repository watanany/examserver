{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Aeson (Value (String), object, (.=))
import Env (Env)
import RIO hiding (Handler (..))
import Servant (
    Application,
    Capture,
    Context (EmptyContext, (:.)),
    Delete,
    DeleteNoContent,
    Get,
    Handler (Handler),
    JSON,
    Post,
    PostCreated,
    Proxy (Proxy),
    Put,
    QueryParam,
    QueryParams,
    ReqBody,
    Server,
    ServerError (errBody),
    ServerT,
    err400,
    err401,
    err404,
    err409,
    err500,
    hoistServerWithContext,
    serveWithContext,
    (:<|>) ((:<|>)),
    (:>),
 )

type RootApi = Get '[JSON] Value

type HealthzApi = "healthz" :> Get '[JSON] Value

type Api =
    RootApi
        :<|> HealthzApi

api :: Proxy Api
api = Proxy

buildApp :: Env -> Application
buildApp env =
    serveWithContext api context (buildServer env)
  where
    context = EmptyContext

buildServer :: Env -> Server Api
buildServer env = hoistServerWithContext api context (switchHandler env) server
  where
    context = Proxy :: Proxy '[]

switchHandler :: Env -> RIO Env a -> Handler a
switchHandler env action =
    Handler $
        ExceptT $
            try $
                runRIO env action

server :: ServerT Api (RIO Env)
server =
    root
        :<|> healthz

root :: RIO Env Value
root = healthz

healthz :: RIO Env Value
healthz = return $ object ["status" .= String "ok"]
