{-# LANGUAGE OverloadedStrings #-}

module Messages
  ( parsePlayerMessage
  , encodeServerMessage
  , PlayerMessage(..)
  , ServerMessage(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Exts (fromList)

data PlayerMessage
  = ConnectionRequest Text
  | KeyChange Int
              Bool
  deriving (Show)

parsePlayerMessage :: ByteString -> Maybe PlayerMessage
parsePlayerMessage str = do
  result <- decode str
  messageType <- flip parseMaybe result $ flip (.:) "type" :: Maybe String
  case messageType of
    "connection_request" -> parse1 result ConnectionRequest
    "keychange" -> parse2 result KeyChange
    _ -> Nothing

parse1 :: FromJSON a => Object -> (a -> b) -> Maybe b
parse1 val with = with <$> (flip parseMaybe val $ flip (.:) "0")

parse2 :: (FromJSON a, FromJSON b) => Object -> (a -> b -> c) -> Maybe c
parse2 val with =
  flip parseMaybe val $ \obj -> do
    arg1 <- obj .: "0"
    arg2 <- obj .: "1"
    return $ with arg1 arg2

data ServerMessage =
  Connected Text
  deriving (Show)

encodeServerMessage :: ServerMessage -> ByteString
encodeServerMessage msg =
  encode $
  case msg of
    Connected username -> helper "connected" [("0", toJSON username)]
  where
    helper messageType values =
      Object . fromList $ ("type", String messageType) : values
