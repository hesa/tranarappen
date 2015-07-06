{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.Time.Clock
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
import System.Directory

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Control.Monad.Catch as E
import qualified Data.JSON.Schema as Schema
import qualified Rest.Resource as R

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "schema")

data TeamWithMembers = TeamWithMembers { teamWithMembersUuid :: UUID
                                       , teamWithMembersName :: Text
                                       , teamWithMembersCreated :: UTCTime
                                       , teamWithMembersClubUuid :: UUID
                                       , teamWithMembersMemberUuids :: [UUID]
                                       }

instance ToJSON TeamWithMembers where
    toJSON (TeamWithMembers uuid name created clubUuid memberUuids) =
        object [ "uuid" .= uuid
               , "name" .= name
               , "created" .= created
               , "clubUuid" .= clubUuid
               , "members" .= memberUuids ]

mkGenericJSON [t|Club|]
mkGenericJSON [t|Member|]
mkGenericJSON [t|Team|]
mkGenericJSON [t|TrainingPhase|]

mkJsonType ''Club def { derivedPrefix = "publish", removeFields = ["created", "uuid"] }
mkJsonType ''Member def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }
mkJsonType ''Team def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }
mkJsonType ''TrainingPhase def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }

newtype App a = App { unApi :: ReaderT ConnectionPool IO a } deriving (Applicative, Functor, Monad, E.MonadCatch, MonadIO, E.MonadThrow)

deriving instance Generic Club
deriving instance Typeable Club
deriving instance Generic Member
deriving instance Typeable Member
deriving instance Generic Team
deriving instance Typeable Team
deriving instance Generic TrainingPhase
deriving instance Typeable TrainingPhase

deriving instance Generic ClubComposite
deriving instance Typeable ClubComposite

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

-- runQuery :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m) => SqlPersistT m b -> m b
runQuery a = ask >>= runSqlPool a

type ClubUuid = UUID
type MemberUuid = UUID
type TeamUuid = UUID
type TrainingPhaseUuid = UUID
type VideoUuid = UUID

data ClubComposite = ClubComposite { clubCompositeName :: Text
                                   , clubCompositeMembers :: [Member]
                                   , clubCompositeTeams :: [TeamWithMembers]
                                   , clubCompositeTrainingPhases :: [TrainingPhase] }

-- mkGenericJSON [t|ClubComposite|]

instance ToJSON ClubComposite where
    toJSON (ClubComposite name members teams trainingPhases) =
        object [ "name" .= name
               , "members" .= members
               , "teams" .= teams
               , "trainingPhases" .= trainingPhases ]

instance Schema.JSONSchema ClubComposite where
    schema _ = Schema.Object [ Schema.Field "name" True Schema.Any -- TODO
                             , Schema.Field "members" True Schema.Any -- TODO
                             , Schema.Field "teams" True Schema.Any -- TODO
                             , Schema.Field "trainingPhases" True Schema.Any -- TODO
                             ]

data AppError = Conflict deriving Typeable

instance ToResponseCode AppError where
    toResponseCode Conflict = 409

instance Schema.JSONSchema AppError where
    schema _ = Schema.Object []

instance ToJSON AppError where
    toJSON _ = object []

-- conflictInsert :: (MonadBaseControl IO m, PersistEntity b, MonadReader (Pool SqlBackend) m, MonadIO m, (~) * (PersistEntityBackend b) SqlBackend) => b -> m (Either (Reason AppError) b)
conflictInsert e = do
    result <- runQuery (insertUnique e)
    return $ case result of
        Just _ -> Right e
        Nothing -> Left (CustomReason (DomainReason Conflict))

router :: Router App App
router = root -/ route clubsR
              --/ route clubsMembersR
              --/ route clubsTeamsR
              --/ route clubsTrainingPhasesR
              --/ route clubsVideosR
              --/ route clubsUploadR
              -/ route clubCompositeR
  where
    clubsR :: Resource App (ReaderT ClubUuid App) ClubUuid () Void
    clubsR = mkResourceReader { R.create = Just create
                              , R.get = Just get
                              , R.list = const list
                              , R.name = "clubs"
                              , R.remove = Just remove
                              , R.schema = withListing () (unnamedSingleRead id)
                              , R.update = Just update}
      where
        create :: Handler App
        create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishClub -> do
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            let club = Club uuid (publishClubName publishClub) now
            ExceptT $ runSql $ conflictInsert club
        get :: Handler (ReaderT ClubUuid App)
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just club <- lift $ runSql $ runQuery (Database.Persist.get (ClubKey uuid))
            return (Right club)
        list :: ListHandler App
        list = mkListing jsonO $ \_ -> lift $ runSql $ do
            clubs <- runQuery (selectList [] [Asc ClubName])
            return (map entityVal clubs)
        remove :: Handler (ReaderT ClubUuid App)
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ runSql $ runQuery (delete (ClubKey uuid))
            return (Right ())
        update :: Handler (ReaderT ClubUuid App)
        update = mkInputHandler (jsonI . jsonO) $ \publishClub -> ExceptT $ do
            uuid <- ask
            lift $ runSql $ do
                runQuery (Database.Persist.update (ClubKey uuid) [ ClubName =. publishClubName publishClub ])
                club <- runQuery (Database.Persist.get (ClubKey uuid))
                return $ Right club
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
            clubUuid <- ask
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            let member = Member uuid (publishMemberName publishMember) now clubUuid (publishMemberTeamUuid publishMember)
            lift $ runSql $ runQuery (insert member)
            return (Right member)
        get :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just member <- lift $ lift $ runSql $ runQuery (Database.Persist.get (MemberKey uuid))
            return (Right member)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ do
                members <- runQuery (selectList [MemberClubUuid ==. uuid] [Asc MemberName])
                return (map entityVal members)
        remove :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ lift $ runSql $ do
                runQuery $ do
                    deleteWhere [MemberClubUuid ==. uuid]
                    deleteWhere [TrainingPhaseClubUuid ==. uuid]
                    deleteWhere [TeamClubUuid ==. uuid]
                    delete (MemberKey uuid)
            return (Right ())
        update :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (MemberKey uuid) [ MemberName =. publishMemberName publishMember, MemberTeamUuid =. publishMemberTeamUuid publishMember])
                member <- runQuery (Database.Persist.get (MemberKey uuid))
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
        create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTeam -> ExceptT $ do
            clubUuid <- ask
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            let team = Team uuid (publishTeamName publishTeam) now clubUuid
            lift $ runSql $ conflictInsert team
        get :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just team <- lift $ lift $ runSql $ runQuery (Database.Persist.get (TeamKey uuid))
            return (Right team)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ do
                teams <- runQuery (selectList [TeamClubUuid ==. uuid] [Asc TeamName])
                return (map entityVal teams)
        remove :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ lift $ runSql $ runQuery (delete (TeamKey uuid))
            return (Right ())
        update :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (TeamKey uuid) [TeamName =. publishTeamName publishTeam])
                team <- runQuery (Database.Persist.get (TeamKey uuid))
                return $ Right team
    clubsTrainingPhasesR :: Resource (ReaderT ClubUuid App) (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App)) TrainingPhaseUuid () Void
    clubsTrainingPhasesR = mkResourceReader { R.create = Just create
                                            , R.get = Just get
                                            , R.list = const list
                                            , R.name = "training-phases"
                                            , R.remove = Just remove
                                            , R.schema = withListing () (unnamedSingleRead id)
                                            , R.update = Just update }
      where
        create :: Handler (ReaderT ClubUuid App)
        create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTrainingPhase -> ExceptT $ do
            clubUuid <- ask
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            lift $ runSql $ do
                let trainingPhase = TrainingPhase uuid (publishTrainingPhaseName publishTrainingPhase) now clubUuid
                conflictInsert trainingPhase
        get :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            Just trainingPhase <- lift $ lift $ runSql $ runQuery (Database.Persist.get (TrainingPhaseKey uuid))
            return (Right trainingPhase)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ do
                trainingPhases <- runQuery (selectList [TrainingPhaseClubUuid ==. uuid] [Asc TrainingPhaseName])
                return (map entityVal trainingPhases)
        remove :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \_ uuid -> ExceptT $ do
            lift $ lift $ runSql $ runQuery (delete (TrainingPhaseKey uuid))
            return (Right ())
        update :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishTrainingPhase -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (TrainingPhaseKey uuid) [TrainingPhaseName =. publishTrainingPhaseName publishTrainingPhase])
                trainingPhases <- runQuery (Database.Persist.get (TrainingPhaseKey uuid))
                return $ Right trainingPhases
    clubsVideosR :: Resource (ReaderT ClubUuid App) (ReaderT VideoUuid (ReaderT ClubUuid App)) VideoUuid Void Void
    clubsVideosR = mkResourceReader { R.get = Just get
                                    , R.name = "videos"
                                    , R.schema = noListing (unnamedSingleRead id)
                                    }
      where
        get :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        get = mkIdHandler fileO $ \() uuid -> ExceptT $ do
            file <- liftIO $ BL.readFile ("videos/" ++ (toString uuid))
            return (Right (file, "", False)) -- TODO
    clubsUploadR :: Resource (ReaderT ClubUuid App) (ReaderT VideoUuid (ReaderT ClubUuid App)) VideoUuid Void Void
    clubsUploadR = mkResourceReader { R.create = Just create
                                    , R.get = Just get
                                    , R.name = "upload"
                                    , R.schema = noListing (unnamedSingleRead id)
                                    }
      where
        create :: Handler (ReaderT ClubUuid App)
        create = mkInputHandler (fileI . stringO) $ \video -> ExceptT $ do
            clubUuid <- ask
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            let upload = Video uuid Nothing now clubUuid
            liftIO $ BL.writeFile ("upload/" ++ toString uuid) video
            lift $ runSql $ runQuery $ insert upload
            return (Right (toString uuid))
        get :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        get = mkIdHandler stringO $ \() uuid -> ExceptT $ do
            upload <- lift $ lift $ runSql $ runQuery (Database.Persist.get (VideoKey uuid))
            case upload of
                Nothing -> return (Left NotFound)
                Just (Video { videoSuccess = Nothing }) -> return (Right "Processing")
                Just (Video { videoSuccess = Just False }) -> return (Right "Failure")
                Just (Video { videoSuccess = Just True }) -> return (Right "Success") -- TODO: 303 See Other
    clubCompositeR :: Resource App (ReaderT UUID App) UUID Void Void
    clubCompositeR = mkResourceReader { R.get = Just get
                                      , R.name = "club-composite"
                                      , R.schema = noListing (unnamedSingleRead id) }
      where
        get :: Handler (ReaderT UUID App)
        get = mkIdHandler jsonO $ \_ uuid -> ExceptT $ lift $ runSql $ do
            Just club <- runQuery (Database.Persist.get (ClubKey uuid))
            members <- runQuery (selectList [] [Asc MemberName])
            teams <- runQuery (selectList [] [Asc TeamName])
            trainingPhases <- runQuery (selectList [] [Asc TrainingPhaseName])
            teamWithMembers <- flip mapM teams $ \teamEntity -> do
                let team = entityVal teamEntity
                members <- runQuery (selectList [MemberTeamUuid ==. Just (teamUuid team)] [Asc MemberName])
                return (teamWithMembers team (map entityVal members))
            let clubComposite = ClubComposite (clubName club) (map entityVal members) teamWithMembers (map entityVal trainingPhases)
            return (Right clubComposite)

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]

main :: IO ()
main = withPool 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    forkIO (uploadThread pool)
    run 3000 (apiToApplication (\(App r) -> runReaderT r pool) api)

uploadThread :: ConnectionPool -> IO ()
uploadThread pool = forever $ do
    threadDelay (10 * 10^6)
    flip runReaderT pool $ do
        uploads <- runQuery (selectList [VideoSuccess ==. Nothing] [])
        flip mapM_ uploads $ \uploadEntity -> do
            let Video uuid _ _ _ = entityVal uploadEntity
            liftIO (copyFile ("upload/" ++ (toString uuid)) ("videos/" ++ (toString uuid)))
            runQuery (Database.Persist.update (entityKey uploadEntity) [VideoSuccess =. Just True])

teamWithMembers :: Team -> [Member] -> TeamWithMembers
teamWithMembers (Team { teamUuid = uuid
                      , teamName = name
                      , teamCreated = created
                      , teamClubUuid = clubUuid
                      }) members = TeamWithMembers uuid name created clubUuid
                                       (map memberUuid members)
