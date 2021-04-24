{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App.Server (
    runApp,
    app,
) where

import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Network.Wai.Handler.Warp (Port, run)
import Servant


type API =
    "projects" :> Get '[JSON] [Project]
        :<|> "projects" :> ReqBody '[JSON] CreateProject :> Post '[JSON] ProjectId
        :<|> "projects" :> Capture "project-id" ProjectId :> Delete '[JSON] ()


newtype ProjectId = ProjectId
    { unProjectId :: Int
    }
    deriving (Eq, Ord, Show)
    deriving (FromHttpApiData, ToJSON, FromJSON) via Int


data Project = Project
    { projectId :: ProjectId
    , projectName :: String
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype CreateProject = CreateProject
    { createProjectName :: String
    }
    deriving (Eq, Show)
    deriving (FromJSON) via String


runApp :: Port -> IO ()
runApp port = run port app


app :: Application
app = serve api server


api :: Proxy API
api = Proxy


server :: Server API
server =
    getProjects
        :<|> createProject
        :<|> deleteProject
  where
    getProjects :: Handler [Project]
    getProjects = liftIO $ do
        projMap <- readIORef appState
        pure $ uncurry Project <$> Map.toList projMap

    createProject :: CreateProject -> Handler ProjectId
    createProject (CreateProject newName) = do
        projMap <- liftIO $ readIORef appState
        if newName `elem` projMap
            then throwError $ err409{errBody = "Failed to create project: name already exists"}
            else
                liftIO $
                    atomicModifyIORef'
                        appState
                        ( \projMap_ ->
                            let newProjId = case Map.lookupMax projMap_ of
                                    Just (maxId, _) -> maxId
                                    Nothing -> ProjectId 0
                             in ( Map.insert newProjId newName projMap_
                                , newProjId
                                )
                        )

    deleteProject :: ProjectId -> Handler ()
    deleteProject projId = do
        projMap <- liftIO $ readIORef appState
        if Map.notMember projId projMap
            then throwError $ err404{errBody = "Failed to delete project: ID doesn't exist"}
            else liftIO $ atomicModifyIORef appState (\projMap_ -> (Map.delete projId projMap_, ()))


appState :: IORef (Map ProjectId String)
appState = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE appState #-}
