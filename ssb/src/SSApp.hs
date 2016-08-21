{-# LANGUAGE RecordWildCards #-}

module SSApp where

import Control.Concurrent (newChan, writeChan, readChan, forkIO)
import Data.Aeson
import Control.Monad (forever)
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.ByteString.Lazy as LBS

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
