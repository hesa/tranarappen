{-# LANGUAGE DataKinds #-}
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
import Generics.Generic.Aeson
import GHC.Generics
import Lambdatrade hiding (Conflict)
import Network.Wai.Handler.Warp
import Rest
import Rest.Api
import Rest.Driver.Wai
import System.Directory

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Database.Esqueleto as E
import qualified Control.Monad.Catch as C
import qualified Data.JSON.Schema as Schema
import qualified Rest.Resource as R

import Other

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "schema")

mkGenericJSON [t|Club|]
mkGenericJSON [t|Member|]
mkGenericJSON [t|Team|]
mkGenericJSON [t|TrainingPhase|]
mkGenericJSON [t|Video|]

mkJsonType ''Club def { derivedPrefix = "publish", removeFields = ["created", "uuid"] }
mkJsonType ''Member def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }
mkJsonType ''Team def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }
mkJsonType ''TrainingPhase def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "uuid"] }
mkJsonType ''Video def { derivedPrefix = "publish", removeFields = ["clubUuid", "created", "published", "status", "uuid"] }

newtype App a = App { unApi :: ReaderT ConnectionPool IO a } deriving (Applicative, Functor, Monad, C.MonadCatch, MonadIO, C.MonadThrow)

-- TODO: Refactor when we upgrade to GHC 7.10.
newtype WithMemberUuids a = WithMemberUuids { unWithMemberUuids :: (WithField "memberUuids" [UUID] a) } deriving (FromJSON, Schema.JSONSchema, ToJSON, Typeable)
newtype WithTeamUuids a = WithTeamUuids { unWithTeamUuids :: (WithField "teamUuids" [UUID] a) } deriving (FromJSON, Schema.JSONSchema, ToJSON, Typeable)
newtype WithTrainingPhaseUuids a = WithTrainingPhaseUuids { unWithTrainingPhaseUuids :: (WithField "trainingPhaseUuids" [UUID] a) } deriving (FromJSON, Schema.JSONSchema, ToJSON, Typeable)
newtype WithVideoUuids a = WithVideoUuids { unWithVideoUuids :: (WithField "videoUuids" [UUID] a) } deriving (FromJSON, Schema.JSONSchema, ToJSON, Typeable)

deriving instance Generic Club
deriving instance Typeable Club
deriving instance Generic Member
deriving instance Typeable Member
deriving instance Generic Team
deriving instance Typeable Team
deriving instance Generic TrainingPhase
deriving instance Typeable TrainingPhase
deriving instance Generic Video
deriving instance Typeable Video
deriving instance Generic ClubComposite
deriving instance Typeable ClubComposite

deriving instance Generic VideoStatus

instance ToJSON VideoStatus where
    toJSON = gtoJson

instance FromJSON VideoStatus where
    parseJSON = gparseJson

instance Schema.JSONSchema VideoStatus where
    schema = Schema.gSchema

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

-- runQuery :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m) => SqlPersistT m b -> m b
runQuery a = ask >>= runSqlPool a

type ClubUuid = UUID
type MemberUuid = UUID
type TeamUuid = UUID
type TrainingPhaseUuid = UUID
type VideoUuid = UUID

data ClubComposite = ClubComposite { clubCompositeUuid :: ClubUuid
                                   , clubCompositeName :: Text
                                   , clubCompositeCreated :: UTCTime
                                   , clubCompositeMembers :: [WithVideoUuids Member]
                                   , clubCompositeTeams :: [WithVideoUuids (WithMemberUuids Team)]
                                   , clubCompositeTrainingPhases :: [WithVideoUuids TrainingPhase]
                                   , clubCompositeVideos :: [Video] }

mkGenericJSON [t|ClubComposite|]

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

data VideoListAccessor = AllVideos
                       | InstructionalVideos
                       | VideosByMember MemberUuid
                       | VideosByTeam TeamUuid
                       | VideosByTrainingPhase TrainingPhaseUuid

-- getClubs :: ReaderT ConnectionPool IO [WithMemberUuids (WithTeamUuids (WithTrainingPhaseUuids (WithVideoUuids Club)))]
getClubs = do
    clubs <- runQuery $ selectList [] [Asc ClubName]
    mapM (getClub . Left . entityVal) clubs

-- getClub :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => Either Club UUID -> m (WithMemberUuids (WithTeamUuids (WithTrainingPhaseUuids (WithVideoUuids Club))))
getClub value = do
    (uuid, club) <- case value of
        Left club -> return (clubUuid club, club)
        Right uuid -> do
            Just club <- runQuery (Database.Persist.get (ClubKey uuid))
            return (uuid, club)
    members <- getMembers uuid Nothing
    teams <- getTeams uuid
    trainingPhases <- getTrainingPhases uuid
    videos <- getVideos uuid AllVideos
    return (WithMemberUuids (WithField (map (memberUuid . withFieldBase . unWithVideoUuids) members) (WithTeamUuids (WithField (map (teamUuid . withFieldBase . unWithMemberUuids . withFieldBase . unWithVideoUuids) teams) (WithTrainingPhaseUuids (WithField (map (trainingPhaseUuid . withFieldBase . unWithVideoUuids) trainingPhases) (WithVideoUuids (WithField (map videoUuid videos) club))))))))

-- getMembers :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Maybe UUID -> m [WithVideoUuids Member]
getMembers clubUuid mbTeamUuid = do
    members <- runQuery $ flip selectList [Asc MemberName] $ case mbTeamUuid of
        Just teamUuid -> [MemberClubUuid ==. clubUuid, MemberTeamUuid ==. Just teamUuid]
        Nothing -> [MemberClubUuid ==. clubUuid]
    mapM (getMember clubUuid . Left . entityVal) members

-- getMember :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Member UUID -> m (WithVideoUuids Member)
getMember clubUuid value = do
    (uuid, member) <- case value of
        Left member -> return (memberUuid member, member)
        Right uuid -> do
            Just member <- runQuery (Database.Persist.get (MemberKey uuid))
            return (uuid, member)
    videos <- getVideos clubUuid (VideosByMember uuid)
    return (WithVideoUuids (WithField (map videoUuid videos) member))

-- getTeams :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m [WithVideoUuids (WithMemberUuids Team)]
getTeams clubUuid = do
    teams <- runQuery (selectList [TeamClubUuid ==. clubUuid] [Asc TeamName])
    mapM (getTeam clubUuid . Left . entityVal) teams

-- getTeam :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Team UUID -> m (WithVideoUuids (WithMemberUuids Team))
getTeam clubUuid value = do
    (uuid, team) <- case value of
        Left team -> return (teamUuid team, team)
        Right uuid -> do
            Just team <- runQuery (Database.Persist.get (TeamKey uuid))
            return (uuid, team)
    members <- getMembers clubUuid (Just uuid)
    videos <- getVideos clubUuid (VideosByTeam uuid)
    return (WithVideoUuids (WithField (map videoUuid videos) (WithMemberUuids (WithField (map (memberUuid . withFieldBase . unWithVideoUuids) members) team))))

-- getTrainingPhases :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m [WithVideoUuids TrainingPhase]
getTrainingPhases clubUuid = do
    trainingPhases <- runQuery (selectList [TrainingPhaseClubUuid ==. clubUuid] [Asc TrainingPhaseName])
    mapM (getTrainingPhase clubUuid . Left . entityVal) trainingPhases

-- getTrainingPhase :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either TrainingPhase UUID -> m (WithVideoUuids TrainingPhase)
getTrainingPhase clubUuid value = do
    (uuid, trainingPhase) <- case value of
        Left trainingPhase -> return (trainingPhaseUuid trainingPhase, trainingPhase)
        Right uuid -> do
            Just trainingPhase <- runQuery (Database.Persist.get (TrainingPhaseKey uuid))
            return (uuid, trainingPhase)
    videos <- getVideos clubUuid (VideosByTrainingPhase uuid)
    return (WithVideoUuids (WithField (map videoUuid videos) trainingPhase))

-- getVideos :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> VideoListAccessor -> m [Video]
getVideos clubUuid accessor = do
    videos <- runQuery $ E.select $ E.from $ \video -> do
        E.where_ (video E.^. VideoStatus E.==. E.val Complete)
        E.where_ (video E.^. VideoClubUuid E.==. E.val clubUuid)
        case accessor of
            AllVideos -> return ()
            InstructionalVideos -> E.where_ (E.isNothing $ video E.^.VideoMemberUuid)
            VideosByMember memberUuid -> E.where_ (video E.^. VideoMemberUuid E.==. E.val (Just memberUuid))
            VideosByTeam teamUuid -> E.where_ (E.exists . E.from $ \member -> do
                E.where_ (member E.^. MemberTeamUuid E.==. E.val (Just teamUuid))
                E.where_ (video E.^. VideoMemberUuid E.==. E.just (member E.^. MemberUuid)))
            VideosByTrainingPhase trainingPhaseUuid -> E.where_ (video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid)
        E.orderBy [E.desc (video E.^. VideoPublished)]
        return video
    return (map entityVal videos)

-- getVideo :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m Video
getVideo uuid = do
    Just video <- runQuery (Database.Persist.get (VideoKey uuid))
    return video

router :: Router App App
router = root -/ route clubsR
              --/ route clubsMembersR
              --/ route clubsTeamsR
              --/ route clubsTrainingPhasesR
              --/ route clubsVideosR
  where
    clubsR :: Resource App (ReaderT ClubUuid App) ClubUuid () Void
    clubsR = mkResourceReader { R.create = Just create
                              , R.get = Just get
                              , R.list = const list
                              , R.name = "clubs"
                              , R.remove = Just remove
                              , R.schema = withListing () (unnamedSingleRead id)
                              , R.selects = [("composite", composite)]
                              , R.update = Just update }
      where
        create :: Handler App
        create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishClub -> do
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            let club = Club uuid (publishClubName publishClub) now
            result <- lift $ runSql $ conflictInsert club
            case result of
                Right club -> lift (runSql (getClub (Right uuid))) >>= ExceptT . return . Right
                Left e -> ExceptT (return (Left e))
        composite :: Handler (ReaderT ClubUuid App)
        composite = mkIdHandler jsonO $ \() uuid -> ExceptT $ lift $ runSql $ do
            Just club <- runQuery (Database.Persist.get (ClubKey uuid)) -- We only need the name.
            members <- getMembers uuid Nothing
            teams <- getTeams uuid
            trainingPhases <- getTrainingPhases uuid
            videos <- getVideos uuid AllVideos
            return (Right (ClubComposite (clubUuid club) (clubName club) (clubCreated club) members teams trainingPhases videos))
        get :: Handler (ReaderT ClubUuid App)
        get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            club <- lift $ runSql $ getClub (Right uuid)
            return (Right club)
        list :: ListHandler App
        list = mkListing jsonO $ \_ -> lift $ runSql getClubs
        remove :: Handler (ReaderT ClubUuid App)
        remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            lift $ runSql $ runQuery (delete (ClubKey uuid))
            return (Right ())
        update :: Handler (ReaderT ClubUuid App)
        update = mkInputHandler (jsonI . jsonO) $ \publishClub -> ExceptT $ do
            uuid <- ask
            lift $ runSql $ do
                runQuery (Database.Persist.update (ClubKey uuid) [ ClubName =. publishClubName publishClub ])
                club <- getClub (Right uuid)
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
        get = mkInputHandler jsonO $ \_ -> ExceptT $ do
            uuid <- ask
            clubUuid <- lift ask
            member <- lift $ lift $ runSql $ getMember clubUuid (Right uuid)
            return (Right member)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ getMembers uuid Nothing
        remove :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        remove = mkInputHandler jsonO $ \_ -> ExceptT $ do
            uuid <- ask
            clubUuid <- lift ask
            lift $ lift $ runSql $ do
                runQuery $ do
                    deleteWhere [MemberClubUuid ==. clubUuid]
                    deleteWhere [TrainingPhaseClubUuid ==. clubUuid]
                    deleteWhere [TeamClubUuid ==. clubUuid]
                    delete (MemberKey uuid)
            return (Right ())
        update :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
            uuid <- ask
            clubUuid <- lift ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (MemberKey uuid) [ MemberName =. publishMemberName publishMember, MemberTeamUuid =. publishMemberTeamUuid publishMember])
                member <- getMember clubUuid (Right uuid)
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
            uuid <- liftIO nextRandom
            clubUuid <- ask
            now <- liftIO getCurrentTime
            let team = Team uuid (publishTeamName publishTeam) now clubUuid
            result <- lift $ runSql $ conflictInsert team
            case result of
                Right team -> lift (runSql (getTeam clubUuid (Right uuid))) >>= return . Right
                Left e -> return (Left e)
        get :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            clubUuid <- lift ask
            team <- lift $ lift $ runSql $ getTeam clubUuid (Right uuid)
            return (Right team)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ getTeams uuid
        remove :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            lift $ lift $ runSql $ runQuery (delete (TeamKey uuid))
            return (Right ())
        update :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
            uuid <- ask
            clubUuid <- lift ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (TeamKey uuid) [TeamName =. publishTeamName publishTeam])
                team <- getTeam clubUuid (Right uuid)
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
                result <- conflictInsert trainingPhase
                case result of
                    Right team -> getTrainingPhase clubUuid (Right uuid) >>= return . Right
                    Left e -> return (Left e)
        get :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            clubUuid <- lift ask
            trainingPhase <- lift $ lift $ runSql $ getTrainingPhase clubUuid (Right uuid)
            return (Right trainingPhase)
        list :: ListHandler (ReaderT ClubUuid App)
        list = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ getTrainingPhases uuid
        remove :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            lift $ lift $ runSql $ runQuery (delete (TrainingPhaseKey uuid))
            return (Right ())
        update :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishTrainingPhase -> ExceptT $ do
            uuid <- ask
            clubUuid <- lift ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (TrainingPhaseKey uuid) [TrainingPhaseName =. publishTrainingPhaseName publishTrainingPhase])
                trainingPhases <- getTrainingPhase clubUuid (Right uuid)
                return $ Right trainingPhases
    clubsVideosR :: Resource (ReaderT ClubUuid App) (ReaderT VideoUuid (ReaderT ClubUuid App)) VideoUuid VideoListAccessor Void
    clubsVideosR = mkResourceReader { R.actions = [("upload", upload)]
                                    , R.create = Just create
                                    , R.get = Just get
                                    , R.list = list
                                    , R.name = "videos"
                                    , R.schema = withListing AllVideos $ named [ ("uuid", singleRead id)
                                                                               , ("member", listingRead VideosByMember)
                                                                               , ("team", listingRead VideosByTeam)
                                                                               , ("training-phase", listingRead VideosByTrainingPhase)
                                                                               , ("instructional", listing InstructionalVideos) ]
                                    , R.selects = [("download", download)]
                                    }
      where
        create :: Handler (ReaderT ClubUuid App)
        create = mkInputHandler (jsonI . jsonO) $ \publishVideo -> ExceptT $ do
            clubUuid <- ask
            uuid <- liftIO nextRandom
            now <- liftIO getCurrentTime
            lift $ runSql $ do
                let video = Video uuid (publishVideoTrainingPhaseUuid publishVideo) (publishVideoMemberUuid publishVideo) Empty now Nothing clubUuid
                runQuery (insertUnique video)
                return (Right video)
        download :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        download = mkIdHandler fileO $ \() uuid -> ExceptT $ do
            video <- lift $ lift $ runSql $ getVideo uuid
            case videoStatus video of
                Complete -> do
                    file <- liftIO $ BL.readFile ("videos/" ++ (toString uuid))
                    return (Right (file, "", False)) -- TODO
                _ -> return (Left NotFound)
        get :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            video <- lift $ lift $ runSql $ getVideo uuid
            return (Right video)
        list :: VideoListAccessor -> ListHandler (ReaderT ClubUuid App)
        list accessor = mkListing jsonO $ \_ -> lift $ do
            uuid <- ask
            lift $ runSql $ getVideos uuid accessor
        remove :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
            lift $ lift $ runSql $ runQuery (delete (VideoKey uuid))
            return (Right ())
        update :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        update = mkInputHandler (jsonI . jsonO) $ \publishVideo -> ExceptT $ do
            uuid <- ask
            lift $ lift $ runSql $ do
                runQuery (Database.Persist.update (VideoKey uuid) [VideoMemberUuid =. publishVideoMemberUuid publishVideo, VideoTrainingPhaseUuid =. publishVideoTrainingPhaseUuid publishVideo])
                video <- getVideo uuid
                return $ Right video
        upload :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
        upload = mkInputHandler (fileI . stringO . jsonE) $ \bytes -> ExceptT $ do
            uuid <- ask
            video <- lift $ lift $ runSql $ getVideo uuid
            case videoStatus video of
                Empty -> do
                    now <- liftIO getCurrentTime
                    liftIO $ BL.writeFile ("upload/" ++ toString uuid) bytes
                    lift $ lift $ runSql $ runQuery (Database.Persist.update (VideoKey uuid) [ VideoStatus =. Processing ])
                    return (Right ("" :: String))
                _ -> return (Left (CustomReason (DomainReason Conflict)))

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
        uploads <- runQuery (selectList [VideoStatus ==. Processing] [])
        flip mapM_ uploads $ \uploadEntity -> do
            let Video uuid _ _ _ _ _ _ = entityVal uploadEntity
            liftIO (copyFile ("upload/" ++ (toString uuid)) ("videos/" ++ (toString uuid)))
            now <- liftIO getCurrentTime
            runQuery (Database.Persist.update (entityKey uploadEntity) [VideoPublished =. Just now, VideoStatus =. Complete])
