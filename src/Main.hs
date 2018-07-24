module Main where

import Web.Spock
import Web.Spock.Config

import DB
import Web

main :: IO ()
main = do
    pool <- dbpool
    spockCfg <- defaultSpockCfg EmptySession pool EmptyAppState
    runSpock 8080 (spock spockCfg app)

