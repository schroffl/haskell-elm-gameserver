module Subscribers where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Data.List (find)
import Data.Text (Text)
import MessageParser
import qualified Network.WebSockets as WS
import Types

makeSubsVar :: IO SubscribersVar
makeSubsVar = newMVar []

addSubscriber :: Subscriber -> SubscribersVar -> IO ()
addSubscriber sub subsVar = modifyMVar_ subsVar $ return . (:) sub

removeSubscriber :: Text -> SubscribersVar -> IO ()
removeSubscriber username subsVar =
  modifyMVar_ subsVar $ return . filter ((/= username) . fst)

broadcast :: [ServerMessage] -> SubscribersVar -> IO ()
broadcast msgs subsVar = do
  subs <- readMVar subsVar
  sequence_ . map (flip WS.sendTextDatas encodedMsgs) $ map snd subs
  where
    encodedMsgs = map encodeServerMessage msgs

notifySingle :: [ServerMessage] -> Text -> SubscribersVar -> IO ()
notifySingle msgs username subsVar = do
  subs <- readMVar subsVar
  let maybeSub = find ((== username) . fst) subs
  print $ fst <$> maybeSub
  case maybeSub of
    Nothing -> return ()
    Just (_, conn) -> WS.sendTextDatas conn encodedMsgs
  where
    encodedMsgs = map encodeServerMessage msgs
