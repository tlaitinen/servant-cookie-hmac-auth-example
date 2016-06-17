{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Session where

import GHC.Generics
import Data.Aeson as A
import Models
import Data.Time (UTCTime)
import qualified Web.ClientSession as CS
import Data.ByteString
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)

data Session = Session {
    userId  :: UserId,
    expires :: UTCTime
} deriving (Generic)

instance FromJSON Session where
instance ToJSON Session where

decodeSession :: CS.Key -> Text -> Maybe Session
decodeSession k t = CS.decrypt k (B64.decodeLenient $ encodeUtf8 t) >>= (A.decode . LB.fromStrict)

encodeSession :: CS.Key -> Session -> IO Text
encodeSession k s = do
    e <- CS.encryptIO k $ LB.toStrict $ A.encode s
    return $ decodeUtf8 $ B64.encode e

