module Subscribers
  ( Subscriber
  , Subscribers
  , makeMVar
  , addSubscriber
  , removeSubscriber
  , notifySubscriber
  , broadcast
  ) where

import Control.Concurrent
import Control.Exception (catch)
import Messages
import qualified Network.WebSockets as WS

type Subscriber = (Int, WS.Connection)

type Subscribers = [Subscriber]

makeMVar :: IO (MVar Subscribers)
makeMVar = newMVar []

addSubscriber :: WS.Connection -> MVar Subscribers -> IO Subscriber
addSubscriber conn subsVar =
  modifyMVar subsVar $ \subs ->
    let sub = (generateId subs, conn)
    in return $ (sub : subs, sub)

removeSubscriber :: Int -> MVar Subscribers -> IO ()
removeSubscriber subId subsVar =
  modifyMVar_ subsVar $ return . filter ((/= subId) . fst)

generateId :: Subscribers -> Int
generateId [] = 0
generateId subs = (+ 1) . maximum . map fst $ subs

notifySubscriber :: [ServerMessage] -> Subscriber -> IO ()
notifySubscriber msgs (subId, conn) = WS.sendTextDatas conn encodedMsgs
  where
    encodedMsgs = map encodeServerMessage msgs

broadcast :: [ServerMessage] -> MVar Subscribers -> IO ()
broadcast msgs subsVar = sequence_ . map doSend =<< readMVar subsVar
  where
    doSend (subId, conn) =
      WS.sendTextDatas conn encodedMsgs `catch` handleException subsVar subId
    encodedMsgs = map encodeServerMessage msgs

handleException :: MVar Subscribers -> Int -> WS.ConnectionException -> IO ()
handleException subsVar subId _ = removeSubscriber subId subsVar
