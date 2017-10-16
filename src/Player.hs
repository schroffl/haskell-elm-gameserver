{-# LANGUAGE OverloadedStrings #-}

module Player
  ( handshake
  , Player
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Messages
import qualified Network.WebSockets as WS

data Player = Player
  { plUsername :: Text
  } deriving (Show)

send :: WS.Connection -> ServerMessage -> IO ()
send conn = WS.sendTextData conn . encodeServerMessage

update :: PlayerMessage -> Player -> Player
update msg player = player

handshake :: WS.Connection -> IO ()
handshake conn = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMessage of
    Just (ConnectionRequest username) -> do
      print $ username `T.append` " connected."
      send conn (Connected username)
      handlePlayer conn $ Player username
    _ -> handshake conn

handlePlayer :: WS.Connection -> Player -> IO ()
handlePlayer conn player = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMessage of
    Nothing -> handlePlayer conn player
    Just msg -> handlePlayer conn $ update msg player
