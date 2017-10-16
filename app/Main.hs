{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, newMVar)
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified Player as Player
import System.Environment (getArgs)

main :: IO ()
main = do
  port <- parsePort <$> getArgs
  WS.runServer "127.0.0.1" port app

parsePort :: [String] -> Int
parsePort args =
  case args of
    [] -> 9160
    (h:_) -> read h

app :: WS.ServerApp
app pending = do
  conn <- WS.acceptRequest pending
  Player.handshake conn
