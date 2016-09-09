{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types.Status (status400)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import SSApp (runSSApp, withJsonEvents, withHtmlRendering)
import DemoApp (demoApp)

main :: IO ()
main = Warp.run 3000 app

app :: Wai.Application
app = websocketsOr WS.defaultConnectionOptions wsApp backupApp
  where
    wsApp :: WS.ServerApp
    wsApp pending_conn = do
        conn <- WS.acceptRequest pending_conn
        let receive = WS.receiveData conn
            send = WS.sendTextData conn
        runSSApp receive send $
          withJsonEvents . withHtmlRendering <$> demoApp

    backupApp :: Wai.Application
    backupApp request send = send res where
      res = Wai.responseLBS status400 [] "Not a WebSocket request"


