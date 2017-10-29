module Game where

import Action (executeAction)
import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Concurrent.Chan.Unagi
import Control.Monad (forever)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Player
import Types

start :: GameMessageChannel -> SubscribersVar -> IO ()
start (input, output) subsVar = do
  forkIO $ tickThread input =<< getCurrentTime
  gameLoop output subsVar initialWorld
  where
    initialWorld = World {wldPlayers = []}

tickThread :: GameMessageInput -> UTCTime -> IO ()
tickThread input time = do
  threadDelay tickDelay
  newTime <- getCurrentTime
  writeChan input . Tick . convertTime $ diffUTCTime newTime time
  tickThread input newTime
  where
    tickRate = (2 :: Float)
    tickDelay = floor $ (1000 / tickRate) * 1000
    convertTime = fromRational . toRational

gameLoop :: GameMessageOutput -> SubscribersVar -> World -> IO ()
gameLoop output subsVar world = do
  (newWorld, action) <- flip update world <$> readChan output
  executeAction action subsVar
  gameLoop output subsVar newWorld

update :: GameMessage -> World -> (World, Action)
update (Tick dt) world = (world, NoAction)
update (CreatePlayer username) world =
  let newPlayer = Player.initPlayer username
      newWorld = world {wldPlayers = newPlayer : wldPlayers world}
  in (newWorld, NotifyPlayer username Connected)
update (UpdatePlayer username clMsg) world = (newWorld, Bunch actions)
  where
    newWorld = world {wldPlayers = newPlayers}
    (newPlayers, actions) = result
    result =
      unzip . flip map (wldPlayers world) $ \p ->
        if plUsername p == username
          then Player.update clMsg p
          else (p, NoAction)
