{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.IORef
import qualified Data.Text as Text
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.WebSockets
import qualified Data.ByteString as BS
import Network.WebSockets
import qualified Network.Wai.Handler.Warp as Warp

import Lib

main :: IO ()
main = Warp.run 3000 app

template :: Integer -> String -> String
template num btnMsg = "<div>" ++
               "<div>Step " ++ show num ++ "</div>" ++
               "<input type='text'/>"++
               "<button onclick=\"emit('click', 'btn1')\">" ++ btnMsg ++ "</button>" ++
               "</div>"


handleWsConn conn = do
  counter <- newIORef (0 :: Integer)
  btnMsg <- newIORef "Click Me!"
  let render = do msg <- template <$> readIORef counter <*> readIORef btnMsg
                  sendTextData conn $ Text.pack msg
  forkIO $ forever $ do
    msg <- receiveData conn
    let _ = msg :: BS.ByteString
    writeIORef btnMsg "Clicked"
    render
  forever $ do
    render
    modifyIORef counter (1+)
    threadDelay 1000000

app :: Application
app = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        handleWsConn conn

    backupApp :: Application
    backupApp request send = send res where
      res = responseLBS status400 [] "Not a WebSocket request"
