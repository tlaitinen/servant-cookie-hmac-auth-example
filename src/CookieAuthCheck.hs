{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module CookieAuthCheck where
import Data.Text (Text)
import Models
import GHC.Generics
import Data.Typeable
import Config
import qualified Data.List as L
import qualified Web.Cookie as C
import Api.Session (sessionCookieName)
import Data.Time (getCurrentTime)
import Servant
import Session (userId, expires, decodeSession)
import CookieAuth

cookieAuthCheck :: Config -> CookieAuthCheck UserId
cookieAuthCheck cfg = CookieAuthCheck $ \c -> do
    let ct = C.parseCookiesText c
    let k = getKey cfg
    case L.lookup sessionCookieName ct >>= decodeSession k of
        Just s -> do
            now <- getCurrentTime
            return $ if now < expires s
                then Just $ userId s
                else Nothing
        Nothing -> return Nothing
