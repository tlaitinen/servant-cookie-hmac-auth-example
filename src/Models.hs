{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Models where
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Data.Aeson
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Data.Text (Text)
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name                Text
    password            Text
    UniqueUser          name
ApiKey json
    userId              UserId
    accessKey           Text
    secretKey           Text
    UniqueApiKey        accessKey
|]


doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
