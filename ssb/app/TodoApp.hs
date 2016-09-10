{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module TodoApp where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import SSApp (SSApp(SSApp), emit, MonadEmit, emitter)

data AppState = AppState

data Action = Action

data Event = Event
           deriving (Eq, Generic)

instance FromJSON Event
instance ToJSON Event

todoApp :: Monad m => m (SSApp m AppState Action Event H.Html)
todoApp = return $ SSApp initialState handleMsg reduce render where
  initialState = AppState

  handleMsg msg = return ()

  reduce action state = return state

  render AppState = do
    H.div H.! HA.class_ "todomvc-wrapper" $ do
      H.section H.! HA.id "todoapp" $ do
        H.header H.! HA.id "header" $ do
          H.h1 "todos"
          H.input H.! HA.id "new-todo"
                  H.! HA.placeholder "What needs to be done?"
        H.section H.! HA.id "main" $ do
          H.input H.! HA.id "toggle-all"
                  H.! HA.type_ "checkbox"
                  H.! HA.name "toggle"
          H.label H.! HA.for "toggle-all" $ "Mark all as complete"
          H.ul H.! HA.id "todo-list" $ do
            H.li $ H.div H.! HA.class_ "view" $ do
              H.input H.! HA.class_ "toggle"
                      H.! HA.type_ "checkbox"
              H.label "foo"
              H.button H.! HA.class_ "destroy" $ ""
            H.li $ H.div H.! HA.class_ "view" $ do
              H.input H.! HA.class_ "toggle"
                      H.! HA.type_ "checkbox"
              H.label "bar"
              H.button H.! HA.class_ "destroy" $ ""
            H.li $ H.div H.! HA.class_ "view" $ do
              H.input H.! HA.class_ "toggle"
                      H.! HA.type_ "checkbox"
              H.label "baz"
              H.button H.! HA.class_ "destroy" $ ""
          H.footer H.! HA.id "footer" $ do
            H.span H.! HA.id "todo-count" $ do
              H.strong "3"
              "items left"
            H.ul H.! HA.id "filters" $ do
              H.li $ H.a H.! HA.class_ "selected" $ "All"
              H.li $ H.a H.! HA.class_ "" $ "Active"
              H.li $ H.a H.! HA.class_ "" $ "Completed"
            H.button H.! HA.class_ "clear-completed"
                     H.! HA.id "clear-completed"
                     $ "Clear completed"
      H.footer H.! HA.id "info" $ do
        H.p $ do
          "Written by "
          H.a H.! HA.href "https://github.com/edofic" $ "Andraž Bajt"
        H.p $ do
          "Example for "
          H.a H.! HA.href "http://todomvc.com" $ "TodoMVC"
