{-# LANGUAGE RecordWildCards #-}

module SSApp where

import Control.Concurrent (newChan, writeChan, readChan, forkIO)
import Control.Monad (forever)
import Data.Aeson
import Data.IORef (newIORef, writeIORef, readIORef)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Blaze.Html as H

data SSApp state action msg repr =
     SSApp { init :: (action -> IO ()) -> IO state
           , handleMsg :: msg -> (action -> IO ()) -> IO ()
           , reduce :: action -> state -> (action -> IO ()) -> IO state
           , render :: state -> repr
           }

runSSApp :: IO input -> (output -> IO ()) -> SSApp state action input output -> IO ()
runSSApp receive send SSApp{..} = do
  actionQueue <- newChan
  let emit = writeChan actionQueue

  forkIO $ forever $ do
    msg <- receive
    handleMsg msg emit

  state <- init emit >>= newIORef
  forever $ do
    snap <- readIORef state
    send $ render snap
    action <- readChan actionQueue
    reduce action snap emit >>= writeIORef state

withJsonEvents :: FromJSON msg => SSApp state action msg repr ->
                                  SSApp state action LBS.ByteString repr
withJsonEvents app = app{handleMsg=handleMsg'} where
  handleMsg' rawMsg emit = case decode rawMsg of
    Nothing  -> putStrLn $ "Unknown msg received: " ++ show rawMsg
    Just msg -> handleMsg app msg emit

withHtmlRendering :: SSApp state action msg H.Html ->
                     SSApp state action msg LBS.ByteString
withHtmlRendering app = app{render=render'} where
  render' state = renderHtml $ render app state

emit :: String -> H.AttributeValue
emit body = H.preEscapedStringValue $ "emit(" ++ body ++ ")"
