{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, forkIO)
import Messages
import qualified Network.WebSockets as WS
import qualified Player as Pl
import qualified Subscribers as Sub

main :: IO ()
main = do
  subVar <- Sub.makeMVar
  playerListVar <- Pl.makeListMVar
  WS.runServer "0.0.0.0" 9160 $ app subVar playerListVar

app :: MVar Sub.Subscribers -> MVar Pl.Players -> WS.ServerApp
app subVar playerListVar pending = do
  conn <- WS.acceptRequest pending
  maybeMsg <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMsg of
    Just (ConnectionRequest uname) -> do
      playerVar <- Pl.makePlayerMVar uname
      Sub.addSubscriber subVar conn
      Pl.addPlayer playerListVar playerVar
    _ -> return ()
