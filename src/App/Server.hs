{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App.Server (
    runApp,
    app,
) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


data User = User
    { userId :: Int
    , userFirstName :: String
    , userLastName :: String
    }
    deriving (Eq, Show)


$(deriveJSON defaultOptions ''User)


type API = "users" :> Get '[JSON] [User]


runApp :: Port -> IO ()
runApp port = run port app


app :: Application
app = serve api server


api :: Proxy API
api = Proxy


server :: Server API
server = return users


users :: [User]
users =
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]
