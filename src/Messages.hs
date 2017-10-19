{-# LANGUAGE OverloadedStrings #-}

module Messages where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Exts (fromList)
import Vector3

data PlayerMessage =
  ConnectionRequest Text
  deriving (Show)

data ServerMessage =
  Connected
  deriving (Show)

parsePlayerMessage :: ByteString -> Maybe PlayerMessage
parsePlayerMessage str = do
  decoded <- decode str
  messageType <- parseMaybe typeDecoder decoded :: Maybe String
  messageParser <-
    case messageType of
      "connection_request" -> Just $ parse1 ConnectionRequest
      _ -> Nothing
  messageParser decoded
  where
    typeDecoder = flip (.:) "type"
    parse1 with = fmap with . parseMaybe (flip (.:) "0")
    parse2 with =
      parseMaybe $ \obj -> do
        arg1 <- obj .: "0"
        arg2 <- obj .: "1"
        return $ with arg1 arg2

encodeServerMessage :: ServerMessage -> ByteString
encodeServerMessage msg =
  encode $
  case msg of
    Connected -> encodeHelper "connected" []
  where
    encodeHelper msgType args =
      Object . fromList $ ("type", toJSON (msgType :: Text)) : args
