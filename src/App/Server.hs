{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App.Server (
    API,
    Project,
    CreateProject,
    ProjectId,
    runApp,
    app,
    maxCapacity,
) where

import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import GHC.IO (unsafePerformIO)
import Network.Wai.Handler.Warp (Port, run)
import Servant (
    Application,
    Handler,
    NoContent (NoContent),
    Proxy (..),
    Server,
    ServerError (errBody),
    err404,
    err409,
    err500,
    serve,
    throwError,
    (:<|>) ((:<|>)),
 )

import App.Api (API, CreateProject (..), Project (..), ProjectId (..))


runApp :: Port -> IO ()
runApp port = run port app


app :: Application
app = serve (Proxy @API) server


server :: Server API
server =
    getProjects
        :<|> createProject
        :<|> deleteProject
        :<|> reset
  where
    getProjects :: Handler [Project]
    getProjects = liftIO $ do
        projMap <- readIORef appState
        pure $ uncurry Project <$> Map.toList projMap

    createProject :: CreateProject -> Handler ProjectId
    createProject (CreateProject newName) = do
        projMap <- liftIO $ readIORef appState
        if
                | Map.size projMap >= maxCapacity ->
                    throwError $ err500{errBody = "Failed to create project: storage full"}
                | newName `elem` projMap ->
                    throwError $ err409{errBody = "Failed to create project: name already exists"}
                | otherwise ->
                    liftIO $
                        atomicModifyIORef'
                            appState
                            ( \projMap_ ->
                                let newProjId = case Map.lookupMax projMap_ of
                                        Just (ProjectId maxId, _) -> ProjectId (maxId + 1)
                                        Nothing -> ProjectId 0
                                 in ( Map.insert newProjId newName projMap_
                                    , newProjId
                                    )
                            )

    deleteProject :: ProjectId -> Handler NoContent
    deleteProject projId = do
        projMap <- liftIO $ readIORef appState
        if Map.notMember projId projMap
            then throwError $ err404{errBody = "Failed to delete project: ID doesn't exist"}
            else liftIO $ atomicModifyIORef appState (\projMap_ -> (Map.delete projId projMap_, ()))
        pure NoContent

    reset :: Handler NoContent
    reset = do
        liftIO $ writeIORef appState Map.empty
        pure NoContent


appState :: IORef (Map ProjectId String)
appState = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE appState #-}


maxCapacity :: Int
maxCapacity = 5
