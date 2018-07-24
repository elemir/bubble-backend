{-# LANGUAGE ScopedTypeVariables #-}

module Web where

import Control.Lens
import Network.HTTP.Types

import Database.Persist.Sqlite
import Web.Spock

import qualified Web.Spock as S
import qualified Database.Persist.Sqlite as P

import DB

data MySession = EmptySession
data MyAppState = EmptyAppState

app :: SpockM SqlBackend MySession MyAppState ()
app = do
    put root $ do
        (task :: BubbleTask) <- jsonBody'
        sId <-  runQuery' $ insert task
        S.json sId
    put (root <//> var) $ \id -> do
        (task :: BubbleTask) <- jsonBody'
        sId <- runQuery' $ insert $ task & bubbleTaskParent .~ id
        S.json sId
    S.get root $ do
        (tasks :: [Entity BubbleTask]) <- runQuery' $ selectList [BubbleTaskParent ==. Nothing] []
        S.json tasks
    S.get (root <//> var) $ \id -> do
        (rtask :: Maybe RBubbleTask) <- getRBubbleTask id
        case rtask of
          Just task -> S.json rtask
          Nothing -> setStatus status404
    S.delete (root <//> var) $ \(id :: Key BubbleTask) -> 
        runQuery' $ P.delete id
    S.patch (root <//> var) $ \id -> do
        (task :: BubbleTask) <- jsonBody'
        runQuery' $ P.replace id task

