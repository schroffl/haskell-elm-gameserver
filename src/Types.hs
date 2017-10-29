module Types where

import Control.Concurrent (MVar)
import Control.Concurrent.Chan.Unagi (InChan, OutChan)
import Data.Text (Text)
import qualified Network.WebSockets as WS

type GameMessageInput = InChan GameMessage

type GameMessageOutput = OutChan GameMessage

type GameMessageChannel = (GameMessageInput, GameMessageOutput)

data GameMessage
  = Tick Double
  | CreatePlayer Text
  | UpdatePlayer Text
                 ClientMessage
  deriving (Show, Eq)

data World = World
  { wldPlayers :: [Player]
  } deriving (Show, Eq)

data Player = Player
  { plUsername :: Text
  , plKeys :: Keys
  } deriving (Show, Eq)

data ClientMessage
  = ConnectionRequest Text
  | KeyChange Int
              Bool
  deriving (Show, Eq)

data ServerMessage =
  Connected
  deriving (Show, Eq)

data Action
  = NoAction
  | NotifyPlayer Text
                 ServerMessage
  | Broadcast [ServerMessage]
  | Bunch [Action]
  deriving (Show, Eq)

data Keys = Keys
  { keyForward :: Bool
  , keyBackward :: Bool
  , keyLeft :: Bool
  , keyRight :: Bool
  } deriving (Show, Eq)

type Subscriber = (Text, WS.Connection)

type SubscribersVar = MVar [Subscriber]
