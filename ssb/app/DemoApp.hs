{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module DemoApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as Text

import SSApp (SSApp(SSApp))

data AppState = AppState { counter :: Integer
                         , btnMsg :: String
                         , text :: String
                         }

data Action = UpdateButton
            | Bump
            | SetText { newText :: String }

data Event = Click
           | Input { value :: String }
           deriving (Eq, Generic)

instance FromJSON Event
instance ToJSON Event

demoApp :: SSApp AppState Action Event Text.Text
demoApp = SSApp init handleMsg reduce render where
  init emit = do
    forkIO $ forever $ do
      threadDelay 1000000
      emit Bump
    return $ AppState 0 "Click Me!" ""

  handleMsg msg emit = emit $ case msg of
    Click       -> UpdateButton
    Input value -> SetText value

  reduce action state emit = case action of
    UpdateButton -> return state{btnMsg = "No more clicking!"}
    Bump -> return state{counter = counter state + 1}
    SetText newText -> return state{text = newText}


  render AppState{..} = Text.pack $
    "<div>" ++
    "<div>Step " ++ show counter ++ "</div>" ++
    "<input type='text' oninput=\"emit({tag: 'Input', value: this.value})\"/>"++
    "<button onclick=\"emit({tag: 'Click', contents: []})\">" ++ btnMsg ++ "</button>" ++
    "<div style=\"transition: margin-top 1s ease; margin-top: " ++ margin ++ "px;\">" ++
    "Text: " ++ text ++ "</div>" ++
    "</div>"
    where
    margin = if counter `mod` 2 == 0 then "0" else "100"
