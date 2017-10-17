{-# LANGUAGE OverloadedStrings #-}

module Player
  ( handshake
  , Player(plUsername)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Messages
import qualified Network.WebSockets as WS

data Player = Player
  { plUsername :: Text
  , plKeys :: Keys
  } deriving (Show)

data Keys = Keys
  { keyForward :: Bool
  , keyBack :: Bool
  , keyLeft :: Bool
  , keyRight :: Bool
  } deriving (Show)

initPlayer :: Text -> Player
initPlayer username =
  Player {plUsername = username, plKeys = Keys False False False False}

send :: WS.Connection -> ServerMessage -> IO ()
send conn = WS.sendTextData conn . encodeServerMessage

handshake :: WS.Connection -> (Player -> IO ()) -> IO ()
handshake conn addPlayer = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMessage of
    Just (ConnectionRequest username) -> do
      send conn (Connected username)
      addPlayer player
      handlePlayer conn player
      where player = initPlayer username
    _ -> handshake conn addPlayer

handlePlayer :: WS.Connection -> Player -> IO ()
handlePlayer conn player = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  print maybeMessage
  case maybeMessage of
    Nothing -> handlePlayer conn player
    Just msg -> handlePlayer conn $ update msg player

update :: PlayerMessage -> Player -> Player
update msg player =
  case msg of
    KeyChange keyCode newState -> player {plKeys = newKeys}
      where newKeys = updateKeys keyCode newState $ plKeys player
    _ -> player

updateKeys :: Int -> Bool -> Keys -> Keys
updateKeys keyCode newState keys =
  case keyCode of
    87 -> keys {keyForward = newState}
    83 -> keys {keyBack = newState}
    65 -> keys {keyLeft = newState}
    68 -> keys {keyRight = newState}
    _ -> keys
