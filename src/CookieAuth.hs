{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module CookieAuth where
import GHC.Generics
import Data.Typeable
import Servant
import Network.HTTP.Types.Header (hCookie)
import Network.Wai (Request(..))
import Servant.Server.Internal.RoutingApplication (
    addAuthCheck, withRequest, delayedFailFatal)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Control.Monad.IO.Class (liftIO)

data CookieAuthCheck usr = CookieAuthCheck { 
    cookieAuth :: ByteString -> IO (Maybe usr)
} deriving (Generic, Typeable)

data CookieAuth usr deriving (Typeable)
data TokenAuth usr (list :: [*]) a deriving (Typeable)

instance (HasServer api context, HasContextEntry context (CookieAuthCheck usr)) 
    => HasServer (CookieAuth usr :> api) context where
    type ServerT (CookieAuth usr :> api) m = usr -> ServerT api m

    route Proxy context subserver = 
        route (Proxy :: Proxy api) context (subserver `addAuthCheck` check)
        where
            ctx :: CookieAuthCheck usr
            ctx = getContextEntry context
            check = withRequest $ \req -> case L.lookup hCookie $ requestHeaders req of
                Just c -> liftIO (cookieAuth ctx c) >>= \mu -> case mu of
                    Just u -> return u
                    Nothing -> delayedFailFatal err403
                Nothing -> delayedFailFatal err403

