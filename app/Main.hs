{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import qualified Game
import MessageParser
import qualified Network.WebSockets as WS
import qualified Player
import qualified Subscribers as Sub
import Types

main :: IO ()
main = do
  chan@(input, output) <- newChan
  subsVar <- Sub.makeSubsVar
  forkIO $ Game.start chan subsVar
  WS.runServer "0.0.0.0" 9160 $ app input subsVar

app :: GameMessageInput -> SubscribersVar -> WS.ServerApp
app input subsVar pending = do
  conn <- WS.acceptRequest pending
  maybeConnReq <- parseClientMessage <$> WS.receiveData conn
  case maybeConnReq of
    Just (ConnectionRequest username) -> do
      writeChan input $ CreatePlayer username
      Sub.addSubscriber (username, conn) subsVar
      Player.handlePlayer conn input username
    _ -> return ()
