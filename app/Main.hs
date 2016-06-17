module Main where

import           System.IO 
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Models                      (doMigrations)
import           InitialData                 (insertInitialData)
import           Data.Aeson                  (eitherDecode)
import qualified Data.ByteString.Lazy        as LB
import qualified Web.ClientSession           as CS

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    
    
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    key <- CS.getKey keyPath
    let cfg = Config { getPool = pool, getEnv = env, getKey = key }
        logger = setLogger env
    runSqlPool doMigrations pool
    runSqlPool insertInitialData pool
    run port $ logger $ app cfg
    where
        keyPath = "config/client_session_key.aes"
-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
--
-- TODO: use 'readMay' for additional safety.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
