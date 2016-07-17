module Routes where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import Database.Persist.Sql
import Rest
import Rest.Api
import Rest.Dictionary

import qualified Data.ByteString.Lazy as BL
import qualified Database.Esqueleto as E
import qualified Rest.Resource as R

import Other
import Types
import Utilities

router :: Router App App
router = root -/ route compositeR
              -/ route membersR
              -/ route teamsR
              -/ route trainingPhasesR
              -/ route videosR

compositeR :: Resource App (ReaderT () App) () Void Void
compositeR = mkResourceReader { R.get = Just get
                              , R.name = "composite" }
  where
    get :: Handler (ReaderT () App)
    get = mkInputHandler jsonO $ \_ -> ExceptT $ lift $ runSql $ do
        members <- getMembers Nothing
        teams <- getTeams
        trainingPhases <- getTrainingPhases
        return $ Right $ Composite { compositeMembers = members
                                   , compositeTeams = teams
                                   , compositeTrainingPhases = trainingPhases }

membersR :: Resource App (ReaderT MemberUuid App) MemberUuid () Void
membersR = mkResourceReader { R.create = Just create
                            , R.get = Just get
                            , R.list = const list
                            , R.name = "members"
                            , R.remove = Just remove
                            , R.schema = withListing () $ unnamedSingleRead id
                            , R.update = Just update }
  where
    create :: Handler App
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishMember -> ExceptT $ do
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- runSql $ conflictInsert $
                      Member { memberUuid = uuid
                             , memberName = publishMemberName publishMember
                             , memberCreated = now
                             , memberTeamUuid = publishMemberTeamUuid publishMember }
        case result of
            Right _ -> do
                mbMember <- runSql $ getMember $ Right uuid
                case mbMember of
                    Just member -> return $ Right member
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT MemberUuid App)
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbMember <- lift $ runSql $ getMember $ Right uuid
        case mbMember of
            Just member -> return $ Right member
            Nothing -> return $ Left NotFound
    list :: ListHandler App
    list = mkListing jsonO $ \_ -> lift $ do
        runSql $ getMembers Nothing
    remove :: Handler (ReaderT MemberUuid App)
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbMember <- lift $ runSql $ getMember $ Right uuid
        case mbMember of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ do
                        E.delete $ E.from $ \member -> E.where_ (member E.^. MemberUuid E.==. E.val uuid)
                        E.delete $ E.from $ \trainingPhase -> E.where_ (trainingPhase E.^. TrainingPhaseUuid E.==. E.val uuid)
                        E.delete $ E.from $ \team -> E.where_ (team E.^. TeamUuid E.==. E.val uuid)
                        E.deleteKey $ MemberKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT MemberUuid App)
    update = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
        uuid <- ask
        mbMember <- lift $ runSql $ getMember $ Right uuid
        case mbMember of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ E.update $ \member -> do
                        E.set member [ MemberName E.=. E.val (publishMemberName publishMember)
                                     , MemberTeamUuid E.=. E.val (publishMemberTeamUuid publishMember) ]
                        E.where_ (member E.^. MemberUuid E.==. E.val uuid)
                    mbMember <- getMember $ Right uuid
                    case mbMember of
                        Just member -> return $ Right member
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

teamsR :: Resource App (ReaderT TeamUuid App) TeamUuid () Void
teamsR = mkResourceReader { R.create = Just create
                          , R.get = Just get
                          , R.list = const list
                          , R.name = "teams"
                          , R.remove = Just remove
                          , R.schema = withListing () $ unnamedSingleRead id
                          , R.update = Just update }
  where
    create :: Handler App
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTeam -> ExceptT $ do
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- runSql $ conflictInsert $
                      Team { teamUuid = uuid
                           , teamName = publishTeamName publishTeam
                           , teamCreated = now }
        case result of
            Right _ -> do
                mbTeam <- runSql $ getTeam $ Right uuid
                case mbTeam of
                    Just team -> return $ Right team
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT TeamUuid App)
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbTeam <- lift $ runSql $ getTeam $ Right uuid
        case mbTeam of
            Just team -> return $ Right team
            Nothing -> return $ Left NotFound
    list :: ListHandler App
    list = mkListing jsonO $ \_ -> lift $ do
        runSql $ getTeams
    remove :: Handler (ReaderT TeamUuid App)
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbTeam <- lift $ runSql $ getTeam $ Right uuid
        case mbTeam of
            Just _ -> do
                lift $ runSql $ runQuery $ E.deleteKey $ TeamKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT TeamUuid App)
    update = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
        uuid <- ask
        mbTeam <- lift $ runSql $ getTeam $ Right uuid
        case mbTeam of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ E.update $ \team -> do
                        E.set team [TeamName E.=. E.val (publishTeamName publishTeam)]
                        E.where_ (team E.^. TeamUuid E.==. E.val uuid)
                    mbTeam <- getTeam $ Right uuid
                    case mbTeam of
                        Just team -> return $ Right team
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

trainingPhasesR :: Resource App (ReaderT TrainingPhaseUuid App) TrainingPhaseUuid () Void
trainingPhasesR = mkResourceReader { R.create = Just create
                                   , R.get = Just get
                                   , R.list = const list
                                   , R.name = "training-phases"
                                   , R.remove = Just remove
                                   , R.schema = withListing () $ unnamedSingleRead id
                                   , R.update = Just update }
  where
    create :: Handler App
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTrainingPhase -> ExceptT $ do
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- runSql $ conflictInsert $
            TrainingPhase { trainingPhaseUuid = uuid
                          , trainingPhaseName = publishTrainingPhaseName publishTrainingPhase
                          , trainingPhaseCreated = now }
        case result of
            Right _ -> do
                mbTrainingPhase <- runSql $ getTrainingPhase $ Right uuid
                case mbTrainingPhase of
                    Just trainingPhase -> return $ Right trainingPhase
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT TrainingPhaseUuid App)
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbTrainingPhase <- lift $ runSql $ getTrainingPhase $ Right uuid
        case mbTrainingPhase of
            Just trainingPhase -> return $ Right trainingPhase
            Nothing -> return $ Left NotFound
    list :: ListHandler App
    list = mkListing jsonO $ \_ -> lift $ do
        runSql $ getTrainingPhases
    remove :: Handler (ReaderT TrainingPhaseUuid App)
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbTrainingPhase <- lift $ runSql $ getTrainingPhase $ Right uuid
        case mbTrainingPhase of
            Just _ -> do
                lift $ runSql $ runQuery $ E.deleteKey $ TrainingPhaseKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT TrainingPhaseUuid App)
    update = mkInputHandler (jsonI . jsonO) $ \publishTrainingPhase -> ExceptT $ do
        uuid <- ask
        mbTrainingPhase <- lift $ runSql $ getTrainingPhase $ Right uuid
        case mbTrainingPhase of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ E.update $ \trainingPhase -> do
                        E.set trainingPhase [TrainingPhaseName E.=. E.val (publishTrainingPhaseName publishTrainingPhase)]
                        E.where_ (trainingPhase E.^. TrainingPhaseUuid E.==. E.val uuid)
                    mbTrainingPhase <- getTrainingPhase $ Right uuid
                    case mbTrainingPhase of
                        Just trainingPhase -> return $ Right trainingPhase
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

videosR :: Resource App (ReaderT VideoUuid App) VideoUuid VideoListAccessor VideoStatics
videosR = mkResourceReader { R.actions = [("upload", upload)]
                           , R.create = Just create
                           , R.get = Just get
                           , R.list = list
                           , R.name = "videos"
                           , R.remove = Just remove
                           , R.schema = withListing AllVideos $
                                 named [ ("uuid", singleRead id)
                                       , ("member", listingRead VideosByMember)
                                       , ("member-and-training-phase", static MemberAndTrainingPhase)
                                       , ("non-instructional", listing NonInstructionalVideos)
                                       , ("team", listingRead VideosByTeam)
                                       , ("team-and-training-phase", static TeamAndTrainingPhase)
                                       , ("training-phase", listingRead VideosByTrainingPhase)
                                       , ("instructional", listing InstructionalVideos) ]
                           , R.selects = [ ("download", download)
                                         , ("poster", poster) ]
                           , R.statics = statics }
  where
    create :: Handler App
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishVideo -> ExceptT $ do
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- runSql $ conflictInsert $
                      Video { videoUuid = uuid
                            , videoTrainingPhaseUuid = publishVideoTrainingPhaseUuid publishVideo
                            , videoMemberUuid = publishVideoMemberUuid publishVideo
                            , videoStatus = Empty
                            , videoCreated = now
                            , videoPublished = Nothing
                            , videoRecorded = publishVideoRecorded publishVideo }
        case result of
            Right _ -> do
               mbVideo <- runSql $ getVideo uuid
               case mbVideo of
                   Just video -> return $ Right video
                   Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    download :: Handler (ReaderT VideoUuid App)
    download = mkIdHandler fileO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ runSql $ getVideo uuid
        case videoStatus <$> mbVideo of
            Just Complete -> do
                file <- liftIO $ BL.readFile $ "videos/" ++ (toString uuid) ++ ".webm"
                return $ Right (file, "", False) -- TODO (also in "poster")
            _ -> return $ Left NotFound
    get :: Handler (ReaderT VideoUuid App)
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ runSql $ getVideo uuid
        case mbVideo of
            Just video -> return $ Right video
            Nothing -> return $ Left NotFound
    list :: VideoListAccessor -> ListHandler App
    list accessor = mkListing jsonO $ \_ -> lift $ do
        runSql $ getVideos accessor
    poster :: Handler (ReaderT VideoUuid App)
    poster = mkIdHandler fileO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ runSql $ getVideo uuid
        case videoStatus <$> mbVideo of
            Just Complete -> do
                file <- liftIO $ BL.readFile $ "videos/" ++ (toString uuid) ++ ".jpeg"
                return $ Right (file, "", False) -- TODO (also in "download")
            _ -> return $ Left NotFound
    remove :: Handler (ReaderT VideoUuid App)
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ runSql $ getVideo uuid
        case mbVideo of
            Just _ -> do
                lift $ runSql $ runQuery $ E.deleteKey $ VideoKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    statics :: VideoStatics -> Handler App
    statics MemberAndTrainingPhase = let memberUuidParam = Param ["memberUuid"] (Right . fromJust . fromString . fromJust . head)
                                         trainingPhaseParam = Param ["trainingPhaseUuid"] (Right . fromJust . fromString . fromJust . head)
                                         params = memberUuidParam `TwoParams` trainingPhaseParam
                                     in mkHandler (jsonO . mkPar params) $ \env -> ExceptT $ do
        let (memberUuid, trainingPhaseUuid) = param env
        videos <- runSql $ getVideos (VideosByMemberAndTrainingPhase memberUuid trainingPhaseUuid)
        return $ Right videos
    statics TeamAndTrainingPhase = let teamUuidParam = Param ["teamUuid"] (Right . fromJust . fromString . fromJust . head)
                                       trainingPhaseParam = Param ["trainingPhaseUuid"] (Right . fromJust . fromString . fromJust . head)
                                       params = teamUuidParam `TwoParams` trainingPhaseParam
                                   in mkHandler (jsonO . mkPar params) $ \env -> ExceptT $ do
        let (teamUuid, trainingPhaseUuid) = param env
        videos <- runSql $ getVideos (VideosByTeamAndTrainingPhase teamUuid trainingPhaseUuid)
        return $ Right videos
    update :: Handler (ReaderT VideoUuid App)
    update = mkInputHandler (jsonI . jsonO) $ \publishVideo -> ExceptT $ do
        uuid <- ask
        mbVideo <- lift $ runSql $ getVideo uuid
        case mbVideo of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ E.update $ \video -> do
                        E.set video [ VideoMemberUuid E.=. E.val (publishVideoMemberUuid publishVideo)
                                    , VideoTrainingPhaseUuid E.=. E.val (publishVideoTrainingPhaseUuid publishVideo) ]
                        E.where_ (video E.^. VideoUuid E.==. E.val uuid)
                    mbVideo <- getVideo uuid
                    case mbVideo of
                        Just video -> return $ Right video
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound
    upload :: Handler (ReaderT VideoUuid App)
    upload = mkInputHandler (fileI . stringO . jsonE) $ \bytes -> ExceptT $ do
        uuid <- ask
        mbVideo <- lift $ runSql $ getVideo uuid
        case mbVideo of
            Just video -> case videoStatus video of
                Empty -> do
                    now <- liftIO getCurrentTime
                    liftIO $ BL.writeFile ("upload/" ++ toString uuid) bytes
                    lift $ runSql $ runQuery $ E.update $ \video -> do
                        E.set video [VideoStatus E.=. E.val Processing]
                        E.where_ (video E.^. VideoUuid E.==. E.val uuid)
                    return $ Right ("" :: String)
                _ -> return $ Left $ CustomReason $ DomainReason Conflict
            Nothing -> return $ Left NotFound

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]
