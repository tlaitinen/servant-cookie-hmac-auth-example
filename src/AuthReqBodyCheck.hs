{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module AuthReqBodyCheck where
import Data.Text (Text)
import Models
import GHC.Generics
import Data.Typeable
import Config
import qualified Data.List as L
import qualified Web.Cookie as C
import Api.Session (sessionCookieName)
import Database.Persist.Sql
import Data.Time (getCurrentTime)
import Servant
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Session (userId, expires, decodeSession)
import qualified HmacAuth as H
import Data.ByteString (ByteString)
import AuthReqBody

authReqBodyCheck :: Config -> AuthReqBodyCheck UserId
authReqBodyCheck cfg = AuthReqBodyCheck $ \t m -> 
    case H.parse (decodeUtf8 t) of
        Just (a,d) -> do
            mat <- runSqlPool (getBy $ UniqueApiKey a) (getPool cfg)
            case mat of
                Just (Entity _ at) ->
                    return $ if H.verify (apiKeySecretKey at) d m
                        then Just $ apiKeyUserId at
                        else Nothing
                Nothing -> return Nothing
        Nothing -> return Nothing
