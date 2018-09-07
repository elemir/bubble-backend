{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}
module Field where

import Data.Maybe
import GHC.Generics

import Data.Aeson

import Database.Persist.Sqlite
import Database.Persist.TH

import Web.Spock

import qualified DB

data FBubbleTask = FBubbleTask
    { name :: Maybe String
    , descr :: Maybe String
    , cur :: Maybe Int
    , aim :: Maybe Int
    , parent :: Maybe DB.BubbleTaskId
    } deriving (Generic, Show)

instance FromJSON FBubbleTask

infixr 3 =?.

(=?.) :: forall v typ. PersistField typ => EntityField v typ -> Maybe typ -> Maybe (Update v)
(=?.) field = fmap (field =.)

bubbleUpdates :: FBubbleTask -> [Update DB.BubbleTask]
bubbleUpdates task = catMaybes
    [ DB.BubbleTaskName =?. name task
    , DB.BubbleTaskDescr =?. fmap Just (descr task)
    , DB.BubbleTaskCur =?. cur task
    , DB.BubbleTaskAim =?. aim task
    , DB.BubbleTaskParent =?. fmap Just (parent task)
    ]

updateBubbleTask :: Key DB.BubbleTask -> FBubbleTask -> SpockActionCtx ctx SqlBackend sess st ()
updateBubbleTask id task = DB.runQuery' $ update id $ bubbleUpdates task

