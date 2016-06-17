{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Api.Session where

import           Data.Text (Text)
import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           GHC.Generics
import           Data.Time (getCurrentTime, addUTCTime)
import           Data.Aeson
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..), App(..))
import           Models
import           Password
import           Session (encodeSession, Session(..))
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.ByteString (ByteString)
import           Database.Persist.Sql
import           Data.Maybe (listToMaybe, Maybe(..))
import           Web.Cookie (SetCookie(..), renderSetCookie)
import           Data.Default (def)
import qualified Blaze.ByteString.Builder as BB

data Creds = Creds {
    username :: Text,
    password :: Text
} deriving (Generic)

instance FromJSON Creds

type SetCookieResult = Headers '[Header "Set-Cookie" Text] ()


type AuthAPI =
         "login" :> ReqBody '[JSON] Creds :> Post '[JSON] SetCookieResult
    :<|> "logout" :> Post '[JSON] SetCookieResult

authServer :: ServerT AuthAPI App
authServer = login :<|> logout

sessionCookieName :: Text
sessionCookieName = "_SESSION"

setCookieToText :: SetCookie -> Text
setCookieToText = decodeUtf8 . BB.toByteString . renderSetCookie

login :: Creds -> App SetCookieResult
login c = do
    mu <- runDB $ getBy $ UniqueUser $ username c
    case mu of
        Just (Entity uId u) -> do    
            key <- asks getKey    
            now <- liftIO getCurrentTime
            if checkPassword (password c) u
                then do

                    let et = addUTCTime sessionLength now
                    t <- liftIO $ encodeSession key $ Session uId et
                    let sc = def {
                                setCookieName = encodeUtf8 sessionCookieName,
                                setCookieValue = encodeUtf8 t,
                                setCookieExpires = Just et,
                                setCookieHttpOnly = True
                            }
                    return $ addHeader (setCookieToText sc) ()
                else err
        Nothing -> err
    where
        sessionLength = 365 * 86400        
        err = throwError $ err403 { errBody = "Invalid username or password" }
logout :: App SetCookieResult
logout = do
    now <- liftIO getCurrentTime
    return $ addHeader (setCookieToText $ sc now) ()
    where
        sc now = def {
                setCookieName = encodeUtf8 sessionCookieName,
                setCookieValue = encodeUtf8 "",
                setCookieExpires = Just now,
                setCookieHttpOnly = True
            }
 

