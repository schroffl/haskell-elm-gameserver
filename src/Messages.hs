{-# LANGUAGE OverloadedStrings #-}

module Messages
  ( parsePlayerMessage
  , PlayerMessage(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

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

parse1 :: FromJSON a => Object -> (a -> b) -> Maybe b
parse1 val with = with <$> (flip parseMaybe val $ flip (.:) "arg1")

parse2 :: (FromJSON a, FromJSON b) => Object -> (a -> b -> c) -> Maybe c
parse2 val with =
  flip parseMaybe val $ \obj -> do
    arg1 <- obj .: "arg1"
    arg2 <- obj .: "arg2"
    return $ with arg1 arg2
