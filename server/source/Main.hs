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
            -- Quality factor of 3, 10 seconds in (putting a "-ss" parameter to
            -- take the screenshot after a number of seconds will fail to
            -- produce a screenshot when the video is shorter than the numer of
            -- seconds; we should use avprobe/ffprobe to query the length of the
            -- video, probably)
            exitCodePoster <- liftIO $ system $ "avconv -i " ++
                                  ("upload/" ++ toString uuid) ++
                                  " -q:v 3 -vframes 1 " ++
                                  ("videos/" ++ toString uuid ++ ".jpeg")
            case exitCodePoster of
                ExitSuccess -> do
                    -- Stereo, 4/5 in Ogg Vorbis quality, 25/50 in video compression
                    exitCodeVideo <- liftIO $ system $ "avconv -i " ++
                                         ("upload/" ++ toString uuid) ++
                                         " -ac 2 -aq 4 -threads 2 -qmax 25 " ++
                                         ("videos/" ++ toString uuid ++ ".webm")
                    case exitCodeVideo of
                        ExitSuccess -> do
                            -- liftIO $ copyFile ("upload/" ++ toString uuid) ("videos/" ++ toString uuid)
                            now <- liftIO getCurrentTime
                            runQuery $ Database.Persist.update
                                (entityKey uploadEntity)
                                [VideoPublished =. Just now, VideoStatus =. Complete]
                        ExitFailure _ -> do
                            runQuery $ Database.Persist.update
                                (entityKey uploadEntity)
                                [VideoStatus =. Failure]
                ExitFailure _ -> do
                    runQuery $ Database.Persist.update
                        (entityKey uploadEntity)
                        [VideoStatus =. Failure]
