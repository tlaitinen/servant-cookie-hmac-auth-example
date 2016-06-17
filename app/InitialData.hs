{-# LANGUAGE OverloadedStrings #-}
module InitialData where

import Models
import Password
import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Data.Maybe (isNothing)

insertInitialData :: SqlPersistT IO ()
insertInitialData = do
    mu <- getBy $ UniqueUser admin
    uId <- case mu of
        Just (Entity uId _) -> return uId
        Nothing -> do
            u <- liftIO $ setPassword admin (User {
                userName              = admin,
                userPassword          = ""
            })            
            insert u
    ma <- getBy $ UniqueApiKey apiKey
    when (isNothing ma) $ void $ insert $ ApiKey uId apiKey apiSecret
    where
        apiKey = "1234"
        apiSecret = "4321"
        admin = "Admin"

