{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure, exitSuccess)


main :: IO ()
main = do
    ok <- tests
    if ok then exitSuccess else exitFailure


tests :: IO Bool
tests =
    checkParallel $
        Group
            "Test.Example"
            [ ("prop_reverse", prop_reverse)
            ]


prop_reverse :: Property
prop_reverse =
    property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) === xs
