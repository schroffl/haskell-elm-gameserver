{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Player as Player
import System.Environment (getArgs)

type PlayerList = [(WS.Connection, Player.Player)]

main :: IO ()
main = do
  port <- parsePort <$> getArgs
  playerListMvar <- newMVar [] :: IO (MVar PlayerList)
  WS.runServer "0.0.0.0" port $ app playerListMvar

parsePort :: [String] -> Int
parsePort args =
  case args of
    [] -> 9160
    (h:_) -> read h

app :: MVar PlayerList -> WS.ServerApp
app playerListMvar pending = do
  conn <- WS.acceptRequest pending
  Player.handshake conn $ addPlayer playerListMvar conn

addPlayer :: MVar PlayerList -> WS.Connection -> Player.Player -> IO ()
addPlayer playerListMvar conn player = do
  print $ Player.plUsername player `T.append` " connected!"
  modifyMVar_ playerListMvar $ return . (:) (conn, player)
