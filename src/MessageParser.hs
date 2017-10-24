{-# LANGUAGE OverloadedStrings #-}

module MessageParser
  ( parseClientMessage
  , encodeServerMessage
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import GHC.Exts (fromList)
import Types

parseClientMessage :: ByteString -> Maybe ClientMessage
parseClientMessage str = do
  decoded <- decode str
  messageType <- parseMaybe (flip (.:) "type") decoded :: Maybe String
  messageParser <-
    case messageType of
      "connection_request" -> Just $ parse1 ConnectionRequest
      "keychange" -> Just $ parse2 KeyChange
      _ -> Nothing
  parseMaybe messageParser decoded
  where
    parse1 with o = with <$> o .: "0"
    parse2 with o = with <$> o .: "0" <*> o .: "1"

encodeServerMessage :: ServerMessage -> ByteString
encodeServerMessage msg =
  encode $
  case msg of
    Connected -> encodeHelper "connected" []
  where
    mapArgs args = zip (map (pack . show) [0 ..]) args
    encodeHelper msgType args =
      Object . fromList $ ("type", toJSON (msgType :: Text)) : mapArgs args
