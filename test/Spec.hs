{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Servant.Client (runClientM)
import System.Exit (exitFailure, exitSuccess)

import App.Client


main :: IO ()
main = do
    ok <- tests
    if ok then exitSuccess else exitFailure


tests :: IO Bool
tests =
    checkParallel $
        Group
            "State Machine Tests"
            [ ("prop_api_tests", prop_api_tests)
            ]


prop_api_tests :: Property
prop_api_tests = withTests 100 $
    property $ do
        clientEnv <- evalIO (initClientEnv 8080)
        let commands =
                [ createProjectCmd clientEnv
                , getProjectsCmd clientEnv
                , deleteProjectCmd clientEnv
                ]

        actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands

        evalIO $ runClientM reset clientEnv
        executeSequential initialState actions


initialState = undefined


createProjectCmd = undefined


getProjectsCmd = undefined


deleteProjectCmd = undefined
