module Main where

import App.Server (runApp)


main :: IO ()
main = do
    putStrLn "Running app at http://localhost:8080/"
    runApp 8080
