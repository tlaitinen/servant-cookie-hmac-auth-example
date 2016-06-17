{-# LANGUAGE OverloadedStrings #-}

module HmacAuth (parse, verify, sign) where
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (Maybe(..))
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString
import Data.Digest.Pure.SHA (hmacSha256, showDigest)
import qualified Data.ByteString.Lazy as LB

realm :: Text
realm = "Example"

sign :: ByteString -> ByteString -> Text
sign s m = T.pack $ showDigest $ hmacSha256 (LB.fromStrict s) 
                                                     (LB.fromStrict m)

verify :: Text -> Text -> ByteString -> Bool
verify s d m = sign (encodeUtf8 s) m == d

parse :: Text -> Maybe (Text,Text)
parse h 
    | r == realm && T.length a > 0 && T.length d > 0 = Just (a,d)
    | otherwise = Nothing
    where
        (r,t)  = T.breakOn " " h 
        (a,d') = T.breakOn ":" (T.drop 1 t)
        d      = T.drop 1 d'
