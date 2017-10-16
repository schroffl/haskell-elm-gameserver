module Player
  ( handshake
  ) where

import Data.Text (Text)
import Messages (PlayerMessage(..), parsePlayerMessage)
import Network.WebSockets as WS

data Player = Player
  { plUsername :: Text
  } deriving (Show)

update :: PlayerMessage -> Player -> Player
update msg player = player

handshake :: WS.Connection -> IO ()
handshake conn = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMessage of
    Just (ConnectionRequest username) ->
      let player = Player username
      in handlePlayer conn player
    _ -> handshake conn

handlePlayer :: WS.Connection -> Player -> IO ()
handlePlayer conn player = do
  maybeMessage <- parsePlayerMessage <$> WS.receiveData conn
  case maybeMessage of
    Nothing -> handlePlayer conn player
    Just msg -> handlePlayer conn $ update msg player
