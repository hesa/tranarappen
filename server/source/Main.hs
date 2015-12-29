{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time.Clock
import Data.UUID
import Database.Persist
import Database.Persist.Sql
import Lambdatrade hiding (Conflict)
import Network.Wai.Handler.Warp
import Rest.Driver.Wai
import System.Directory

import Other
import Routes
import Types
import Utilities

main :: IO ()
main = withPool 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    withAsync (uploadThread pool) $ \async -> do
        link async
        run 3000 $ apiToApplication (\(App r) -> runReaderT r pool) api

uploadThread :: ConnectionPool -> IO ()
uploadThread pool = forever $ do
    threadDelay $ 10 * 10^6
    flip runReaderT pool $ do
        uploads <- runQuery $ selectList [VideoStatus ==. Processing] []
        forM_ uploads $ \uploadEntity -> do
            let Video uuid _ _ _ _ _ _ = entityVal uploadEntity
            liftIO $ copyFile ("upload/" ++ toString uuid) ("videos/" ++ toString uuid)
            now <- liftIO getCurrentTime
            runQuery $ Database.Persist.update
                (entityKey uploadEntity)
                [VideoPublished =. Just now, VideoStatus =. Complete]
