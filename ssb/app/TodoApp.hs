{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module TodoApp where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import SSApp (SSApp(SSApp), emit, MonadEmit, emitter)

data TodoItem = TodoItem { tid :: Integer
                         , text :: String
                         , isDone :: Bool
                         } deriving (Eq, Show)

data AppState = AppState { newTodo :: String
                         , entries :: [TodoItem]
                         } deriving (Eq, Show)

data Action = UpdateTodoInput String
            | SaveNewTodo
            | DestroyTodo Integer
            deriving (Eq, Show)

data Event = EInput { value :: String }
           | EKeyUp { value :: String }
           | EClick { value :: String }
           deriving (Eq, Show, Generic)

instance FromJSON Event
instance ToJSON Event

todoApp :: (MonadEmit Action IO m, MonadIO m) => m (SSApp m AppState Action Event H.Html)
todoApp = return $ SSApp initialState handleMsg reduce render where
  initialState = AppState { newTodo = "conquer the world"
                          , entries = [ TodoItem 1 "fio" True
                                      , TodoItem 2 "bar" False
                                      , TodoItem 3 "baz" True
                                      ]
                          }

  handleMsg msg = do
    emitEv <- (liftIO . ) <$> emitter
    case msg of EInput value -> emitEv $ UpdateTodoInput value
                EKeyUp code -> if code == "Enter"
                               then emitEv SaveNewTodo
                               else return ()
                EClick action -> if (take 8 action) == "destroy-"
                                 then emitEv (DestroyTodo $ read $ drop 8 action)
                                 else return ()


  reduce (UpdateTodoInput value) state = return $ state { newTodo = value }
  reduce SaveNewTodo state = let newItem = TodoItem (maximum (map tid $ entries state) + 1)
                                                    (newTodo state)
                                                    False
                                 newState = state { newTodo = ""
                                                  , entries = entries state ++ [newItem]
                                                  }
                             in  return newState
  reduce (DestroyTodo tid) state = let newEntries = filter p $ entries state
                                       p (TodoItem { tid = tid' }) = tid /= tid'
                                   in  return $ state { entries = newEntries }

  render AppState{..} = do
    H.div H.! HA.class_ "todomvc-wrapper" $ do
      H.section H.! HA.id "todoapp" $ do
        H.header H.! HA.id "header" $ do
          H.h1 "todos"
          H.input H.! HA.id "new-todo"
                  H.! HA.placeholder "What needs to be done?"
                  H.! HA.value (H.stringValue newTodo)
                  H.! HA.oninput (emit "{tag: 'EInput', value: this.value}")
                  H.! HA.onkeyup (emit "{tag: 'EKeyUp', value: event.code}")
        H.section H.! HA.id "main" $ do
          H.input H.! HA.id "toggle-all"
                  H.! HA.type_ "checkbox"
                  H.! HA.name "toggle"
          H.label H.! HA.for "toggle-all" $ "Mark all as complete"
          H.ul H.! HA.id "todo-list" $ forM_ entries $ \entry -> do
            H.li $ H.div H.! HA.class_ "view" $ do
              H.input H.! HA.class_ "toggle"
                      H.! HA.type_ "checkbox"
                      H.! HA.checked (if isDone entry then "true" else "false")
              H.label $ H.string $ text entry
              H.button H.! HA.class_ "destroy"
                       H.! HA.onclick (emit $ "{tag: 'EClick', value: 'destroy-" ++ show (tid entry) ++ "'}")
                       $ ""
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
