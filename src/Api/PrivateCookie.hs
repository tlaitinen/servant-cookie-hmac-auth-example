{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Api.PrivateCookie where

import           Data.Text (Text)
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           GHC.Generics
import           Data.Aeson
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..), App(..))
import           Models
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Database.Persist.Sql
import           Session
import           Data.Time (getCurrentTime)
import qualified Web.Cookie as C
import           Api.Session (sessionCookieName)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import CookieAuth

type PrivateCookieAPI = CookieAuth UserId :>
    (
        "cookieauth" :> ReqBody '[JSON] DataIn :> Post '[JSON] ()
    )

data DataIn = DataIn {
    values :: [Int]
} deriving (Generic, Show)

instance FromJSON DataIn 

postCookieAuth :: UserId -> DataIn -> App ()
postCookieAuth uId d = liftIO $ print ("cookie auth data", d, "from", uId)
    
cookieServer :: ServerT PrivateCookieAPI App
cookieServer authId = postCookieAuth authId


