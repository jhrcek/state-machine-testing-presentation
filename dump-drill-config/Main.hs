{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import App.Api
import Data.Proxy (Proxy (..))
import Servant.Benchmark
import Test.QuickCheck (Arbitrary, arbitrary)

import Servant.Benchmark.Tools.Drill

import qualified Servant.Benchmark.Tools.Drill as Drill


main :: IO ()
main = do
    endpoints <- generate (Proxy @API) myApiGenerator
    let drillSettings =
            Drill.MkSettings
                { concurrency = 2
                , base = "http://localhost:8080"
                , iterations = 10
                , rampup = 10
                }
    Drill.export "benchmark.yml" drillSettings endpoints


myApiGenerator :: Generator API
myApiGenerator =
    ("get projects", 1)
        :|: arbitrary :>: ("create project", 2)
        :|: arbitrary :>: ("delete project", 2)
        :|: ("reset", 1)


instance Arbitrary CreateProject where
    arbitrary = CreateProject <$> arbitrary


instance Arbitrary ProjectId where
    arbitrary = ProjectId <$> arbitrary
