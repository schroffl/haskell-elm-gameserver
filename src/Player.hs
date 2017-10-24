module Player where

import Control.Concurrent.Chan.Unagi (writeChan)
import Control.Monad (forever)
import Data.Text (Text)
import MessageParser
import qualified Network.WebSockets as WS
import Types

initPlayer :: Text -> Player
initPlayer username =
  Player {plUsername = username, plKeys = Keys False False False False}

handlePlayer :: WS.Connection -> GameMessageInput -> Text -> IO ()
handlePlayer conn input username =
  forever $ do
    maybeMsg <- parseClientMessage <$> WS.receiveData conn
    case maybeMsg of
      Nothing -> return ()
      Just msg -> writeChan input $ UpdatePlayer username msg

update :: ClientMessage -> Player -> (Player, Action)
update (KeyChange keyCode newState) player =
  (player {plKeys = newKeys}, NoAction)
  where
    keys = plKeys player
    newKeys =
      case keyCode of
        87 -> keys {keyForward = newState}
        83 -> keys {keyBackward = newState}
        65 -> keys {keyLeft = newState}
        68 -> keys {keyRight = newState}
        _ -> keys
update msg player = (player, NoAction)
