{-# LANGUAGE RecordWildCards #-}

module SSApp where

import Control.Concurrent (newChan, writeChan, readChan, forkIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Monad (forever)

data SSApp state action msg repr =
     SSApp { init :: (action -> IO ()) -> IO state
           , handleMsg :: msg -> (action -> IO ()) -> IO ()
           , reduce :: action -> state -> (action -> IO ()) -> IO state
           , render :: state -> repr
           }

runSSApp :: IO msg -> (msg -> IO ()) -> SSApp state action msg msg -> IO ()
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
