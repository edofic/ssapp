{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module DemoApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import SSApp (SSApp(SSApp), HtmlApp, EventfulHTML, Code(Code), MonadEmit, emitter)

data AppState = AppState { counter :: Integer
                         , btnMsg :: String
                         , text :: String
                         }

data Action = UpdateButton
            | Bump
            | SetText { newText :: String }

data Event f = Click
             | Input { value :: f String }
             deriving (Generic)

deriving instance (Show (f String)) => Show (Event f)
instance (FromJSON (f String)) => FromJSON (Event f)
instance (ToJSON (f String)) => ToJSON (Event f) where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


demoApp :: (MonadIO m, MonadEmit Action IO m) => m (HtmlApp m AppState Action Event)
demoApp = do
  emitEvent <- emitter
  _ <- liftIO $ forkIO $ forever $ do
    threadDelay 1000000
    emitEvent Bump

  return $ SSApp initialState handleMsg reduce render where
    initialState = AppState 0 "Click Me!" "initial text"

    handleMsg msg = do
      emit <- emitter
      liftIO $ emit $ case msg of
        Click       -> UpdateButton
        Input (Identity value) -> SetText value

    reduce action state = do
      emit <- emitter
      case action of
        Bump -> return state{counter = counter state + 1}
        SetText newText -> return state{text = newText}
        UpdateButton -> do
          _ <- liftIO $ forkIO $ do
            threadDelay 1000000
            emit $ SetText "Delayed effect"
          return state{btnMsg = "No more clicking!"}

    render :: AppState -> EventfulHTML (Event Code)
    render AppState{..} act = H.div $ do
      H.div $ "Step " >> H.string (show counter)
      H.input H.! HA.type_ "text"
              H.! HA.oninput  (act $ Input $ Code "this.value")
      H.button H.! HA.onclick (act Click) $
        H.string btnMsg
      let marginTop = "margin-top: " ++
                      (if counter `mod` 2 == 0 then "0" else "100") ++
                      "px;"
          style = H.stringValue $ "transition: margin-top 1s ease;" ++ marginTop
      H.div H.! HA.style style $
        H.string text
