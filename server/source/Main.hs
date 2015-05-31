{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.Typeable
import Data.UUID
import Data.UUID.V4
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Lambdatrade
import Network.Wai.Handler.Warp
import Rest
import Rest.Api
import Rest.Driver.Wai

import Data.Text (Text)

import qualified Data.JSON.Schema as Schema
import qualified Rest.Resource as R

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "schema")

mkGenericJSON [t|Club|]

mkJsonType ''Club def { derivedPrefix = "create", removeFields = ["uuid"] }

newtype App a = App { unApi :: ReaderT ConnectionPool IO a } deriving (Applicative, Functor, Monad)

deriving instance Generic Club
deriving instance Typeable Club

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

router :: Router App App
router = root -/ route clubsR
  where
    clubsR :: Resource App (ReaderT Club App) Club () Void
    clubsR = mkResourceReader { R.create = Just createClubH
                              , R.list = const list
                              , R.name = "clubs"
                              , R.schema = withListing () $ named [] }
    list :: ListHandler App
    list = mkListing jsonO $ \_ -> lift $ runSql $ do
        pool <- ask
        clubs <- runSqlPool (selectList [] [Asc ClubName]) pool
        return (map entityVal clubs)
    createClubH :: Handler App
    createClubH = mkInputHandler (jsonI . jsonO) $ \club -> ExceptT $ do
        r <- runSql (createClub club)
        return $ Right r
    createClub :: CreateClub -> ReaderT ConnectionPool IO Club
    createClub c = do
        uuid <- lift nextRandom
        let club = Club uuid (createClubName c)
        pool <- ask
        runSqlPool (insert club) pool
        return club

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]

main :: IO ()
main = withPool 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    run 3000 (apiToApplication (\(App r) -> runReaderT r pool) api)
