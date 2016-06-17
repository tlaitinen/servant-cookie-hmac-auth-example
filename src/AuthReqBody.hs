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
module AuthReqBody where
import GHC.Generics
import Data.Typeable
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Servant
import Servant.API.ContentTypes    (AllCTUnrender (..))
import Network.HTTP.Types.Header (hContentType,  hAuthorization)
import Network.Wai (Request(..), strictRequestBody)
import Servant.Server.Internal.RoutingApplication (
    addAuthCheck, withRequest, DelayedIO(..), delayedFailFatal)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)

data AuthReqBodyCheck usr = AuthReqBodyCheck { 
    tokenAuth  :: ByteString -> ByteString -> IO (Maybe usr) 
} deriving (Generic, Typeable)

data AuthReqBody usr (list :: [*]) a deriving (Typeable)

instance (HasServer api context, HasContextEntry context (AuthReqBodyCheck usr),
    AllCTUnrender list a) 
    => HasServer (AuthReqBody usr list a :> api) context where
    type ServerT (AuthReqBody usr list a :> api) m = (usr, a) -> ServerT api m

    route Proxy context subserver = 
        route (Proxy :: Proxy api) context (subserver `addAuthCheck` check)
        where
            ctx :: AuthReqBodyCheck usr
            ctx = getContextEntry context
            check = withRequest $ \req -> case L.lookup hAuthorization $ requestHeaders req of
                Just t -> do
                    let contentTypeH = fromMaybe "application/octet-stream"
                         $ L.lookup hContentType $ requestHeaders req
                    body <- liftIO $ strictRequestBody req
                    mu <- liftIO $ tokenAuth ctx t (LB.toStrict body)
                    mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH) 
                        <$> liftIO (return body)
                    case (mu, mrqbody) of
                        (Just u, Just (Right v)) -> return (u, v)
                        (Nothing, _) -> delayedFailFatal err403
                        (_, Nothing) -> delayedFailFatal err415
                        (_, Just (Left e)) -> delayedFailFatal err400 { errBody = cs e }
                Nothing -> delayedFailFatal err403

                


