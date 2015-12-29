module Routes where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Rest
import Rest.Api

import qualified Data.ByteString.Lazy as BL
import qualified Rest.Resource as R

import Other
import Types
import Utilities

router :: Router App App
router = root -/ route clubsR
              --/ route clubsMembersR
              --/ route clubsTeamsR
              --/ route clubsTrainingPhasesR
              --/ route clubsVideosR

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
