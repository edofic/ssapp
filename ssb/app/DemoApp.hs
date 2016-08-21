{-# LANGUAGE RecordWildCards #-}

module DemoApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Data.Text as Text

import SSApp (SSApp(SSApp))

data AppState = AppState { counter :: Integer
                         , btnMsg :: String
                         }

data Action = Click | Bump

demoApp :: SSApp AppState Action Text.Text Text.Text
demoApp = SSApp init handleMsg reduce render where
  init emit = do
    forkIO $ forever $ do
      threadDelay 1000000
      emit Bump
    return $ AppState 0 "Click Me!"

  handleMsg msg emit = emit Click

  reduce action state emit = case action of
    Click -> return state{btnMsg = "No more clicking!"}
    Bump  -> return state{counter = counter state + 1}

  render AppState{..} = Text.pack $
    "<div>" ++
    "<div>Step " ++ show counter ++ "</div>" ++
    "<input type='text'/>"++
    "<button onclick=\"emit('click', 'btn1')\">" ++ btnMsg ++ "</button>" ++
    "</div>"
