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
-- import System.Directory
import System.Exit
import System.Process

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
    threadDelay $ 5 * 10^6
    flip runReaderT pool $ do
        uploads <- runQuery $ selectList [VideoStatus ==. Processing] []
        forM_ uploads $ \uploadEntity -> do
            let Video uuid _ _ _ _ _ _ = entityVal uploadEntity
            -- Stereo, 4/5 in Ogg Vorbis quality, 25/50 in video compression
            exitCode <- liftIO $ system $ "avconv -i " ++
                            ("upload/" ++ toString uuid) ++
                            " -ac 2 -aq 4 -threads 2 -qmax 25 " ++
                            ("videos/" ++ toString uuid ++ ".webm")
            -- liftIO $ copyFile ("upload/" ++ toString uuid) ("videos/" ++ toString uuid)
            case exitCode of
                ExitSuccess -> do
                    now <- liftIO getCurrentTime
                    runQuery $ Database.Persist.update
                        (entityKey uploadEntity)
                        [VideoPublished =. Just now, VideoStatus =. Complete]
                ExitFailure _ -> do
                    runQuery $ Database.Persist.update
                        (entityKey uploadEntity)
                        [VideoStatus =. Failure]
