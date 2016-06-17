module Password (setPassword, checkPassword) where

import Models
import Crypto.PasswordStore
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)

pwdStrength :: Int
pwdStrength = 17

setPassword :: Text -> User -> IO User
setPassword pwd u = do
    p <- makePassword (encodeUtf8 pwd) pwdStrength
    return $ u {
        userPassword = decodeUtf8 p
    }

checkPassword :: Text -> User -> Bool
checkPassword pwd u = verifyPassword (encodeUtf8 pwd) (encodeUtf8 $ userPassword u)

