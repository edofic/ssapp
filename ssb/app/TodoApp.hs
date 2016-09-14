{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TodoApp where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity(Identity))
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import SSApp (HtmlApp, Code(Code), SSApp(SSApp), MonadEmit, emitter)

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
            | ToggleTodo Integer
            deriving (Eq, Show)

data Event f = EInput { value :: f String }
             | EKeyUp { value :: f String }
             | EClick { target :: String }
             deriving (Generic)

deriving instance (Show (f String)) => Show (Event f)
instance (FromJSON (f String)) => FromJSON (Event f)
instance (ToJSON (f String)) => ToJSON (Event f) where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

todoApp :: (MonadEmit Action IO m, MonadIO m) => m (HtmlApp m AppState Action Event)
todoApp = return $ SSApp initialState handleMsg reduce render where
  initialState = AppState { newTodo = "conquer the world"
                          , entries = [ TodoItem 1 "fio" True
                                      , TodoItem 2 "bar" False
                                      , TodoItem 3 "baz" True
                                      ]
                          }

  handleMsg msg = do
    emitEv <- (liftIO . ) <$> emitter
    case msg of EInput (Identity value) -> emitEv $ UpdateTodoInput value
                EKeyUp (Identity code) -> if code == "Enter"
                               then emitEv SaveNewTodo
                               else return ()
                EClick action
                  | take 8 action == "destroy-" ->
                      emitEv (DestroyTodo $ read $ drop 8 action)
                  | take 7 action == "toggle-" ->
                      emitEv (ToggleTodo $ read $ drop 7 action)
                  | otherwise -> return ()



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
  reduce (ToggleTodo tid) state = let newEntries = map f $ entries state
                                      f i@(TodoItem { tid = tid' })
                                        | tid' == tid = i { isDone = not (isDone i) }
                                        | otherwise   = i
                                  in  return $ state { entries = newEntries }

  render AppState{..} act = do
    H.div H.! HA.class_ "todomvc-wrapper" $ do
      H.section H.! HA.id "todoapp" $ do
        H.header H.! HA.id "header" $ do
          H.h1 "todos"
          H.input H.! HA.id "new-todo"
                  H.! HA.placeholder "What needs to be done?"
                  H.! HA.value (H.stringValue newTodo)
                  H.! HA.oninput (act $ EInput $ Code "this.value")
                  H.! HA.onkeyup (act $ EKeyUp $ Code "event.code")
        H.section H.! HA.id "main" $ do
          H.input H.! HA.id "toggle-all"
                  H.! HA.type_ "checkbox"
                  H.! HA.name "toggle"
          H.label H.! HA.for "toggle-all" $ "Mark all as complete"
          H.ul H.! HA.id "todo-list" $ forM_ entries $ \entry -> do
            H.li $ H.div H.! HA.class_ "view" $ do
              let checkedClass = if isDone entry then "checked" else ""
              H.div H.! HA.class_ (H.stringValue $ "toggle " ++ checkedClass)
                    H.! HA.onclick (act $ EClick $ "toggle-" ++ show (tid entry))
                    $ ""
              H.label $ H.string $ text entry
              H.button H.! HA.class_ "destroy"
                       H.! HA.onclick (act $ EClick $ "destroy-" ++ show (tid entry))
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
        H.p $
          "Written by " >>
          (H.a H.! HA.href "https://github.com/edofic" $ "Andraž Bajt")
        H.p $
          "Example for " >>
          (H.a H.! HA.href "http://todomvc.com" $ "TodoMVC")
