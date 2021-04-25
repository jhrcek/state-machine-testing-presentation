{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module App.Api (
    API,
    Project (..),
    ProjectId (..),
    CreateProject (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant (Capture, Delete, FromHttpApiData, Get, JSON, Post, ReqBody, ToHttpApiData, type (:<|>), type (:>))


type API =
    "projects" :> Get '[JSON] [Project]
        :<|> "projects" :> ReqBody '[JSON] CreateProject :> Post '[JSON] ProjectId
        :<|> "projects" :> Capture "project-id" ProjectId :> Delete '[JSON] ()
        :<|> "reset" :> Delete '[JSON] ()


newtype ProjectId = ProjectId
    { unProjectId :: Int
    }
    deriving (Eq, Ord, Show)
    deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via Int


data Project = Project
    { projectId :: ProjectId
    , projectName :: String
    }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


newtype CreateProject = CreateProject
    { createProjectName :: String
    }
    deriving (Eq, Show)
    deriving (FromJSON, ToJSON) via String
