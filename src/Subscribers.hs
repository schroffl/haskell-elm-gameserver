module Subscribers where

import Control.Concurrent
import Messages
import qualified Network.WebSockets as WS

type Subscribers = [WS.Connection]

makeMVar :: IO (MVar Subscribers)
makeMVar = newMVar []

addSubscriber :: MVar Subscribers -> WS.Connection -> IO ()
addSubscriber var conn = modifyMVar_ var $ return . (:) conn

broadcast :: [ServerMessage] -> MVar Subscribers -> IO ()
broadcast msgs subsVar =
  sequence_ . map (flip WS.sendTextDatas encoded) =<< readMVar subsVar
  where
    encoded = map encodeServerMessage msgs
