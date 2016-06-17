{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Api.PrivateAuthReqBody where

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
import           Data.Time (getCurrentTime)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString
import           AuthReqBody

data AuthDataIn = AuthDataIn {
    values :: [Int]
} deriving (Generic, Show)
instance FromJSON AuthDataIn

type PrivateAuthReqBodyAPI = "authbody" :> AuthReqBody UserId '[JSON] AuthDataIn :> Post '[JSON] () 

postAuthData :: (UserId, AuthDataIn) -> App ()
postAuthData (u,ad) = liftIO $ print ("auth req body", ad, "from", u) 

authReqBodyServer :: ServerT PrivateAuthReqBodyAPI App
authReqBodyServer = postAuthData


