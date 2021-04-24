{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}

module App.Client (
    Api.CreateProject (..),
    Api.Project (..),
    Api.ProjectId (..),
    initClientEnv,
    getProjects,
    createProject,
    deleteProject,
    reset,
) where

import Data.Data (Proxy (Proxy))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (Port)
import Servant.API (type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM, Scheme (Http), client, mkClientEnv)

import App.Api as Api (API, CreateProject (..), Project (..), ProjectId (..))


initClientEnv :: Port -> IO ClientEnv
initClientEnv port = do
    let baseUrl = BaseUrl Http "localhost" port ""
    manager <- newManager defaultManagerSettings
    pure $ mkClientEnv manager baseUrl


getProjects :: ClientM [Project]
createProject :: CreateProject -> ClientM ProjectId
deleteProject :: ProjectId -> ClientM ()
reset :: ClientM ()
getProjects
    :<|> createProject
    :<|> deleteProject
    :<|> reset = client (Proxy @API)
