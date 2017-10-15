{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment (getArgs)

type Client = (Text, WS.Connection)

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
  handleClient conn

handleClient :: WS.Connection -> IO ()
handleClient conn =
  forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", you said"
