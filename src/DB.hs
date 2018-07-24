{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module DB where

import Data.Maybe
import GHC.Generics

import Control.Monad.Logger

import Data.Aeson

import Database.Persist.Sqlite
import Database.Persist.TH

import Web.Spock
import Web.Spock.Config

share
    [ mkPersist sqlSettings { mpsGenerateLenses = True }
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
BubbleTask json
    name String
    descr String Maybe
    cur Int
    aim Int
    parent BubbleTaskId Maybe
|]

data RBubbleTask = RBubbleTask
    { name :: String
    , descr :: String
    , cur :: Int
    , aim :: Int
    , subtasks :: [RBubbleTask]
    } deriving (Generic, Show)

instance ToJSON RBubbleTask

rBubbleTask :: BubbleTask -> [RBubbleTask] -> RBubbleTask
rBubbleTask (BubbleTask name descr cur aim _) xs = 
    RBubbleTask name (fromMaybe "" descr) cur aim xs

toRBubbleTask :: [Entity BubbleTask] -> Maybe RBubbleTask
toRBubbleTask (Entity id task:xs) = 
    Just $ rBubbleTask task $ fst $ toRBubbleTask' xs [id]
  where
    toRBubbleTask' :: [Entity BubbleTask] -> [BubbleTaskId] -> ([RBubbleTask], [Entity BubbleTask])
    toRBubbleTask' bts@(Entity id task@(BubbleTask _ _ _ _ (Just pid)):xbts) ps@(pid':xps)
      | pid == pid' =
        let (cbs, rbts) = toRBubbleTask' xbts (id:ps)
            (pbs, rbts') = toRBubbleTask' rbts ps
         in ((rBubbleTask task cbs):pbs, rbts')
      | otherwise = toRBubbleTask' bts xps
    toRBubbleTask' bts [] = ([], bts)
    toRBubbleTask' [] _ = ([], [])
toRBubbleTask [] = Nothing

getRBubbleTask :: Key BubbleTask -> SpockActionCtx ctx SqlBackend sess st (Maybe RBubbleTask)
getRBubbleTask id = fmap toRBubbleTask $ runQuery' $ rawSql "WITH RECURSIVE rtasks(x) AS (SELECT ? UNION ALL SELECT bubble_task.id FROM rtasks, bubble_task WHERE bubble_task.parent = rtasks.x ORDER BY 1 DESC) SELECT ?? FROM bubble_task, rtasks WHERE id = rtasks.x" [PersistInt64 $ fromSqlKey id]

runQuery' :: SqlPersistM val -> SpockActionCtx ctx SqlBackend sess st val
runQuery' q = runQuery $ \conn -> runSqlPersistM q conn

dbpool :: IO (PoolOrConn SqlBackend)
dbpool = do
  let nconnections = 10
  pgpool <- runNoLoggingT $ createSqlitePool "bubble.sqlite" nconnections
  runSqlPool (runMigration migrateAll) pgpool
  return $ PCPool pgpool

