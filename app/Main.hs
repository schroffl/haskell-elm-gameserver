{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import qualified Game
import MessageParser
import qualified Network.WebSockets as WS
import qualified Player
import Types
       (ClientMessage(..), GameMessage(..), GameMessageInput)

main :: IO ()
main = do
  chan@(input, output) <- newChan
  forkIO $ Game.start chan
  WS.runServer "0.0.0.0" 9160 $ app input

app :: GameMessageInput -> WS.ServerApp
app input pending = do
  conn <- WS.acceptRequest pending
  maybeConnReq <- parseClientMessage <$> WS.receiveData conn
  case maybeConnReq of
    Just (ConnectionRequest username) -> do
      writeChan input $ CreatePlayer username
      Player.handlePlayer conn input username
    _ -> return ()
