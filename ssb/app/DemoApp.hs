{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DemoApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import SSApp (SSApp(SSApp), emit)

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

demoApp :: SSApp AppState Action Event H.Html
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

  render AppState{..} = H.div $ do
    H.div $ "Step " >> H.string (show counter)
    H.input H.! HA.type_ "text"
            H.! HA.oninput  (emit "{tag: 'Input', value: this.value}")
    H.button H.! HA.onclick (emit "{tag: 'Click', contents: []}") $
      H.string btnMsg
    let marginTop = "margin-top: " ++
                    (if counter `mod` 2 == 0 then "0" else "100") ++
                    "px;"
        style = H.stringValue $ "transition: margin-top 1s ease;" ++ marginTop
    H.div H.! HA.style style $
      H.string text
