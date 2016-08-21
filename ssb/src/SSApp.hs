{-# LANGUAGE RecordWildCards #-}

module SSApp where

import Control.Concurrent (newChan, writeChan, readChan, forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Blaze.Html as H

data SSApp m state action msg repr =
     SSApp { init :: (action -> m ()) -> m state
           , handleMsg :: msg -> (action -> m ()) -> m ()
           , reduce :: action -> state -> (action -> m ()) -> m state
           , render :: state -> repr
           }

runSSApp :: MonadIO m => IO msg ->
                         (repr -> IO ()) ->
                         SSApp m state action msg repr ->
                         m ()
runSSApp receive send SSApp{..} = do
  queue <- liftIO newChan
  let emit ev = liftIO $ writeChan queue (Right ev)

  liftIO $ forkIO $ forever $ do
    msg <- receive
    writeChan queue $ Left msg

  initialState <- init emit
  liftIO $ send $ render initialState
  let loop snap = do entry <- liftIO $ readChan queue
                     case entry of
                       Left msg -> handleMsg msg emit >> loop snap
                       Right ev -> do newState <- reduce ev snap emit
                                      liftIO $ send $ render newState
                                      loop newState
  loop initialState

withJsonEvents :: (FromJSON msg, MonadIO m) =>
                  SSApp m state action msg repr ->
                  SSApp m state action LBS.ByteString repr
withJsonEvents app = app{handleMsg=handleMsg'} where
  handleMsg' rawMsg emit = case decode rawMsg of
    Nothing  -> liftIO $ putStrLn $ "Unknown msg received: " ++ show rawMsg
    Just msg -> handleMsg app msg emit

withHtmlRendering :: SSApp m state action msg H.Html ->
                     SSApp m state action msg LBS.ByteString
withHtmlRendering app = app{render=render'} where
  render' state = renderHtml $ render app state

emit :: String -> H.AttributeValue
emit body = H.preEscapedStringValue $ "emit(" ++ body ++ ")"
