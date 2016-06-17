{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
module Api (app) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models
import           CookieAuth
import           CookieAuthCheck (cookieAuthCheck)
import           AuthReqBody
import           AuthReqBodyCheck (authReqBodyCheck)
import           Api.Session
import           Api.PrivateCookie
import           Api.PrivateAuthReqBody

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)


files :: Application
files = serveDirectory "assets"

type AppAPI = AuthAPI :<|> PrivateCookieAPI :<|> PrivateAuthReqBodyAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

authContext :: Config -> Context ((AuthReqBodyCheck UserId) ': (CookieAuthCheck UserId) ': '[])
authContext cfg = (authReqBodyCheck cfg) :. (cookieAuthCheck cfg) :. EmptyContext
app :: Config -> Application
app cfg =
    serveWithContext appApi (authContext cfg) (srv authServer 
        :<|> srv cookieServer 
        :<|> srv authReqBodyServer
        :<|> files) 
    where
        srv f = enter (convertApp cfg) f
