module GameLoop where

import Control.Concurrent
import Control.Monad (forever)
import Messages
import Player
import qualified Subscribers as Sub

tickLength :: Int
tickLength = 5000 * 1000

gameLoop :: MVar Sub.Subscribers -> MVar Players -> IO ()
gameLoop subVar playerListVar = do
  threadDelay tickLength
  allMessages <- modifyPlayers runSimulation =<< readMVar playerListVar
  let messages = foldr (:) [] allMessages
  print messages

runSimulation :: Player -> (Player, [ServerMessage])
runSimulation oldPlayer = (newPlayer, deriveMessages oldPlayer newPlayer)
  where
    newPlayer = oldPlayer

modifyPlayers :: (a -> (a, b)) -> [MVar a] -> IO [b]
modifyPlayers f playerVars =
  sequence $ map (flip modifyMVar $ return . f) playerVars
