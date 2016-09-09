{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module SSApp where

import Control.Concurrent (newChan, writeChan, readChan, forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Aeson
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Blaze.Html as H

class (Monad m) => MonadEmit e n m | m -> n where
  emitter :: m (e -> n ())

type IOEmitter e = ReaderT (e -> IO ())

instance (MonadIO m) => MonadEmit e IO (IOEmitter e m) where
  emitter = ask

data SSApp m state action msg repr =
     SSApp { initialState :: state
           , handleMsg :: msg ->  m ()
           , reduce :: action -> state -> m state
           , render :: state -> repr
           }

runSSApp :: MonadIO m => IO msg ->
                         (repr -> IO ()) ->
                         IOEmitter action m (SSApp (IOEmitter action m) state action msg repr) ->
                         m ()
runSSApp receive send mapp = do
  queue <- liftIO newChan
  let emit ev = liftIO $ writeChan queue (Right ev)

  liftIO $ forkIO $ forever $ do
    msg <- receive
    writeChan queue $ Left msg

  SSApp{..} <- runReaderT mapp emit
  liftIO $ send $ render initialState
  let loop snap = do entry <- liftIO $ readChan queue
                     case entry of
                       Left msg -> handleMsg msg >> loop snap
                       Right ev -> do newState <- reduce ev snap
                                      liftIO $ send $ render newState
                                      loop newState
  runReaderT (loop initialState) emit

withJsonEvents :: (FromJSON msg, MonadIO m) =>
                  SSApp m state action msg repr ->
                  SSApp m state action LBS.ByteString repr
withJsonEvents app = app{handleMsg=handleMsg'} where
  handleMsg' rawMsg = case decode rawMsg of
    Nothing  -> liftIO $ putStrLn $ "Unknown msg received: " ++ show rawMsg
    Just msg -> handleMsg app msg

withHtmlRendering :: SSApp m state action msg H.Html ->
                     SSApp m state action msg LBS.ByteString
withHtmlRendering app = app{render=render'} where
  render' state = renderHtml $ render app state

emit :: String -> H.AttributeValue
emit body = H.preEscapedStringValue $ "emit(" ++ body ++ ")"
