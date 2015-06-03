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
mkGenericJSON [t|Member|]
mkGenericJSON [t|Team|]

mkJsonType ''Club def { derivedPrefix = "publish", removeFields = ["uuid"] }
mkJsonType ''Member def { derivedPrefix = "publish", removeFields = ["clubId", "uuid"] }
mkJsonType ''Team def { derivedPrefix = "publish", removeFields = ["clubId", "uuid"] }

newtype App a = App { unApi :: ReaderT ConnectionPool IO a } deriving (Applicative, Functor, Monad)

deriving instance Generic Club
deriving instance Typeable Club
deriving instance Generic Member
deriving instance Typeable Member
deriving instance Generic Team
deriving instance Typeable Team

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

type ClubUuid = UUID
type MemberUuid = UUID
type TeamUuid = UUID

router :: Router App App
router = root -/ route clubsR
              --/ route clubsMembersR
              --/ route clubsTeamsR
  where
    clubsR :: Resource App (ReaderT UUID App) UUID () Void
    clubsR = mkResourceReader { R.create = Just create
                              , R.get = Just get
                              , R.list = const list
                              , R.name = "clubs"
                              , R.schema = withListing () (unnamedSingleRead id) }
      where
        create :: Handler App
        create = mkInputHandler (jsonI . jsonO) $ \publishClub -> ExceptT $ do
            runSql $ do
                uuid <- lift nextRandom
                let club = Club uuid (publishClubName publishClub)
                pool <- ask
                runSqlPool (insert club) pool
                return $ Right club
        get :: Handler (ReaderT UUID App)
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just club <- lift $ runSql $ do
                pool <- ask
                runSqlPool (Database.Persist.get (ClubKey uuid)) pool
            return (Right club)
        list :: ListHandler App
        list = mkListing jsonO $ \_ -> lift $ runSql $ do
            pool <- ask
            clubs <- runSqlPool (selectList [] [Asc ClubName]) pool
            return (map entityVal clubs)
    clubsMembersR :: Resource (ReaderT ClubUuid App) (ReaderT MemberUuid (ReaderT ClubUuid App)) MemberUuid () Void
    clubsMembersR = mkResourceReader { R.create = Just create
                                   , R.get = Just get
                                   , R.list = const list
                                   , R.name = "members"
                                   , R.remove = Just remove
                                   , R.schema = withListing () (unnamedSingleRead id)
                                   , R.update = Just update }
      where
        create :: Handler (ReaderT ClubUuid App)
        create = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
            clubId <- ask
            lift $ runSql $ do
                uuid <- lift nextRandom
                let member = Member uuid (publishMemberName publishMember) clubId Nothing
                pool <- ask
                runSqlPool (insert member) pool
                return $ Right member
        get :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just member <- lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (Database.Persist.get (MemberKey uuid)) pool
            return (Right member)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ do
                pool <- ask
                members <- runSqlPool (selectList [MemberClubId ==. uuid] [Asc MemberName]) pool
                return (map entityVal members)
        remove :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (delete (MemberKey uuid)) pool
            return (Right ())
        update :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (Database.Persist.update (MemberKey uuid) [MemberName =. publishMemberName publishMember]) pool
                member <- runSqlPool (Database.Persist.get (MemberKey uuid)) pool
                return $ Right member
    clubsTeamsR :: Resource (ReaderT ClubUuid App) (ReaderT TeamUuid (ReaderT ClubUuid App)) TeamUuid () Void
    clubsTeamsR = mkResourceReader { R.create = Just create
                                   , R.get = Just get
                                   , R.list = const list
                                   , R.name = "teams"
                                   , R.remove = Just remove
                                   , R.schema = withListing () (unnamedSingleRead id)
                                   , R.update = Just update }
      where
        create :: Handler (ReaderT ClubUuid App)
        create = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
            clubId <- ask
            lift $ runSql $ do
                uuid <- lift nextRandom
                let team = Team uuid (publishTeamName publishTeam) clubId
                pool <- ask
                runSqlPool (insert team) pool
                return $ Right team
        get :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just team <- lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (Database.Persist.get (TeamKey uuid)) pool
            return (Right team)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ do
                pool <- ask
                teams <- runSqlPool (selectList [TeamClubId ==. uuid] [Asc TeamName]) pool
                return (map entityVal teams)
        remove :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (delete (TeamKey uuid)) pool
            return (Right ())
        update :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                pool <- ask
                runSqlPool (Database.Persist.update (TeamKey uuid) [TeamName =. publishTeamName publishTeam]) pool
                team <- runSqlPool (Database.Persist.get (TeamKey uuid)) pool
                return $ Right team

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]

main :: IO ()
main = withPool 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    run 3000 (apiToApplication (\(App r) -> runReaderT r pool) api)
