{-# LANGUAGE OverloadedStrings #-}

module Player where

import Control.Concurrent
import Control.Monad (forever)
import Data.Text (Text)
import Messages
import qualified Network.WebSockets as WS

type Players = [MVar Player]

makeListMVar :: IO (MVar Players)
makeListMVar = newMVar []

addPlayer :: MVar Players -> MVar Player -> IO ()
addPlayer playerListVar playerVar =
  modifyMVar_ playerListVar $ return . (:) playerVar

data Player = Player
  { plUsername :: Text
  } deriving (Show)

makePlayerMVar :: Text -> IO (MVar Player)
makePlayerMVar username = newMVar $ Player {plUsername = username}

handlePlayer :: WS.Connection -> MVar Player -> IO ()
handlePlayer conn playerVar =
  forever $ do
    maybeMsg <- parsePlayerMessage <$> WS.receiveData conn
    return ()

deriveMessages :: Player -> Player -> [ServerMessage]
deriveMessages oldPlayer newPlayer =
  []

-- applyInput :: Player -> Player
-- applyVelocity :: Player -> Player
-- deriveMessages :: Player -> Player -> [ServerMessage]
