{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Data (Data(..))
import Data.Maybe
import Data.Monoid
import Data.IORef
import Network.HTTP.Types
import GHC.Generics

import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson

import Web.Spock
import Web.Spock.Config

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Data.Text as T
import qualified Web.Spock as S
import qualified Database.Persist.Sqlite as P

data MySession = EmptySession
data MyAppState = EmptyAppState

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BubbleTask json
    name String
    descr String Maybe
    cur Int
    aim Int
|]

dbpool :: IO (PoolOrConn SqlBackend)
dbpool = do
  let nconnections = 10
  pgpool <- runNoLoggingT $ createSqlitePool "bubble.sqlite" nconnections
  runSqlPool (runMigration migrateAll) pgpool
  return $ PCPool pgpool

main :: IO ()
main = do
    pool <- dbpool
    spockCfg <- defaultSpockCfg EmptySession pool EmptyAppState
    runSpock 8080 (spock spockCfg app)

runQuery' :: SqlPersistM val -> SpockActionCtx ctx SqlBackend sess st val
runQuery' q = runQuery $ \conn -> runSqlPersistM q conn

app :: SpockM SqlBackend MySession MyAppState ()
app = do
    put root $ do
        (task :: BubbleTask) <- jsonBody'
        sId <-  runQuery' $ insert task
        S.json sId
    S.get root $ do
        (tasks :: [Entity BubbleTask]) <- runQuery' $ selectList [] []
        S.json tasks
    S.get (root <//> var) $ \id -> do
        (mtask :: Maybe BubbleTask) <- runQuery' $ P.get id
        case mtask of
            Just task -> S.json $ Entity id task
            Nothing -> setStatus status404
    S.delete (root <//> var) $ \(id :: Key BubbleTask) -> 
        runQuery' $ P.delete id
    S.patch (root <//> var) $ \id -> do
        (task :: BubbleTask) <- jsonBody'
        runQuery' $ P.replace id task

