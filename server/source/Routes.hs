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
                          , R.schema = withListing () $ unnamedSingleRead id
                          , R.selects = [("composite", composite)]
                          , R.update = Just update }
  where
    create :: Handler App
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishClub -> do
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- lift $ runSql $ conflictInsert $
                      Club { clubUuid = uuid
                           , clubName = publishClubName publishClub
                           , clubCreated = now }
        case result of
            Right _ -> do
               mbClub <- lift $ runSql $ getClub $ Right uuid
               case mbClub of
                   Just club -> ExceptT $ return $ Right club
                   Nothing -> ExceptT $ return $ Left NotFound
            Left e -> ExceptT $ return $ Left e
    composite :: Handler (ReaderT ClubUuid App)
    composite = mkIdHandler jsonO $ \() uuid -> ExceptT $ lift $ runSql $ do
        mbClub <- runQuery $ E.get $ ClubKey uuid
        case mbClub of
            Just club -> do
                members <- getMembers uuid Nothing
                teams <- getTeams uuid
                trainingPhases <- getTrainingPhases uuid
                return $ Right $ ClubComposite { clubCompositeUuid = clubUuid club
                                               , clubCompositeName = clubName club
                                               , clubCompositeCreated = clubCreated club
                                               , clubCompositeMembers = members
                                               , clubCompositeTeams = teams
                                               , clubCompositeTrainingPhases = trainingPhases }
            Nothing -> return $ Left NotFound
    get :: Handler (ReaderT ClubUuid App)
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbClub <- lift $ runSql $ getClub $ Right uuid
        case mbClub of
            Just club -> return $ Right club
            Nothing -> return $ Left NotFound
    list :: ListHandler App
    list = mkListing jsonO $ \_ -> lift $ runSql getClubs -- TODO: rangeOffset, rangeCount
    remove :: Handler (ReaderT ClubUuid App)
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbClub <- lift $ runSql $ getClub $ Right uuid
        case mbClub of
            Just _ -> do
                lift $ runSql $ runQuery $ do
                    E.delete $ E.from $ \member -> E.where_ (member E.^. MemberClubUuid E.==. E.val uuid)
                    E.delete $ E.from $ \trainingPhase -> E.where_ (trainingPhase E.^. TrainingPhaseClubUuid E.==. E.val uuid)
                    E.delete $ E.from $ \team -> E.where_ (team E.^. TeamClubUuid E.==. E.val uuid)
                    E.delete $ E.from $ \video -> E.where_ (video E.^. VideoClubUuid E.==. E.val uuid)
                    E.deleteKey $ ClubKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT ClubUuid App)
    update = mkInputHandler (jsonI . jsonO) $ \publishClub -> ExceptT $ do
        uuid <- ask
        mbClub <- lift $ runSql $ getClub $ Right uuid
        case mbClub of
            Just _ -> do
                lift $ runSql $ do
                    runQuery $ E.update $ \club -> do
                        E.set club [ClubName E.=. E.val (publishClubName publishClub)]
                        E.where_ (club E.^. ClubUuid E.==. E.val uuid)
                    mbClub <- getClub $ Right uuid
                    case mbClub of
                        Just club -> return $ Right club
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

clubsMembersR :: Resource (ReaderT ClubUuid App) (ReaderT MemberUuid (ReaderT ClubUuid App)) MemberUuid () Void
clubsMembersR = mkResourceReader { R.create = Just create
                                 , R.get = Just get
                                 , R.list = const list
                                 , R.name = "members"
                                 , R.remove = Just remove
                                 , R.schema = withListing () $ unnamedSingleRead id
                                 , R.update = Just update }
  where
    create :: Handler (ReaderT ClubUuid App)
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishMember -> ExceptT $ do
        clubUuid <- ask
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- lift $ runSql $ conflictInsert $
                      Member { memberUuid = uuid
                             , memberName = publishMemberName publishMember
                             , memberCreated = now
                             , memberClubUuid = clubUuid
                             , memberTeamUuid = publishMemberTeamUuid publishMember }
        case result of
            Right _ -> do
                mbMember <- lift $ runSql $ getMember clubUuid $ Right uuid
                case mbMember of
                    Just member -> return $ Right member
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- lift ask
        mbMember <- lift $ lift $ runSql $ getMember clubUuid $ Right uuid
        case mbMember of
            Just member -> return $ Right member
            Nothing -> return $ Left NotFound
    list :: ListHandler (ReaderT ClubUuid App)
    list = mkListing jsonO $ \_ -> lift $ do -- TODO: rangeOffset, rangeCount
        uuid <- ask
        lift $ runSql $ getMembers uuid Nothing
    remove :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- lift ask
        mbMember <- lift $ lift $ runSql $ getMember clubUuid $ Right uuid
        case mbMember of
            Just _ -> do
                lift $ lift $ runSql $ do
                    runQuery $ do
                        E.delete $ E.from $ \member -> E.where_ (member E.^. MemberClubUuid E.==. E.val uuid)
                        E.delete $ E.from $ \trainingPhase -> E.where_ (trainingPhase E.^. TrainingPhaseClubUuid E.==. E.val uuid)
                        E.delete $ E.from $ \team -> E.where_ (team E.^. TeamClubUuid E.==. E.val uuid)
                        E.deleteKey $ MemberKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT MemberUuid (ReaderT ClubUuid App))
    update = mkInputHandler (jsonI . jsonO) $ \publishMember -> ExceptT $ do
        uuid <- ask
        clubUuid <- lift ask
        mbMember <- lift $ lift $ runSql $ getMember clubUuid $ Right uuid
        case mbMember of
            Just _ -> do
                lift $ lift $ runSql $ do
                    runQuery $ E.update $ \member -> do
                        E.set member [ MemberName E.=. E.val (publishMemberName publishMember)
                                     , MemberTeamUuid E.=. E.val (publishMemberTeamUuid publishMember) ]
                        E.where_ (member E.^. MemberUuid E.==. E.val uuid)
                    mbMember <- getMember clubUuid $ Right uuid
                    case mbMember of
                        Just member -> return $ Right member
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

clubsTeamsR :: Resource (ReaderT ClubUuid App) (ReaderT TeamUuid (ReaderT ClubUuid App)) TeamUuid () Void
clubsTeamsR = mkResourceReader { R.create = Just create
                               , R.get = Just get
                               , R.list = const list
                               , R.name = "teams"
                               , R.remove = Just remove
                               , R.schema = withListing () $ unnamedSingleRead id
                               , R.update = Just update }
  where
    create :: Handler (ReaderT ClubUuid App)
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTeam -> ExceptT $ do
        uuid <- liftIO nextRandom
        clubUuid <- ask
        now <- liftIO getCurrentTime
        result <- lift $ runSql $ conflictInsert $
                      Team { teamUuid = uuid
                           , teamName = publishTeamName publishTeam
                           , teamCreated = now
                           , teamClubUuid = clubUuid }
        case result of
            Right _ -> do
                mbTeam <- lift $ runSql $ getTeam clubUuid $ Right uuid
                case mbTeam of
                    Just team -> return $ Right team
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- lift ask
        mbTeam <- lift $ lift $ runSql $ getTeam clubUuid $ Right uuid
        case mbTeam of
            Just team -> return $ Right team
            Nothing -> return $ Left NotFound
    list :: ListHandler (ReaderT ClubUuid App)
    list = mkListing jsonO $ \_ -> lift $ do -- TODO: rangeOffset, rangeCount
        uuid <- ask
        lift $ runSql $ getTeams uuid
    remove :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- ask
        mbTeam <- lift $ lift $ runSql $ getTeam clubUuid $ Right uuid
        case mbTeam of
            Just _ -> do
                lift $ lift $ runSql $ runQuery $ E.deleteKey $ TeamKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT TeamUuid (ReaderT ClubUuid App))
    update = mkInputHandler (jsonI . jsonO) $ \publishTeam -> ExceptT $ do
        uuid <- ask
        clubUuid <- lift ask
        mbTeam <- lift $ lift $ runSql $ getTeam clubUuid $ Right uuid
        case mbTeam of
            Just _ -> do
                lift $ lift $ runSql $ do
                    runQuery $ E.update $ \team -> do
                        E.set team [TeamName E.=. E.val (publishTeamName publishTeam)]
                        E.where_ (team E.^. TeamUuid E.==. E.val uuid)
                    mbTeam <- getTeam clubUuid $ Right uuid
                    case mbTeam of
                        Just team -> return $ Right team
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

clubsTrainingPhasesR :: Resource (ReaderT ClubUuid App) (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App)) TrainingPhaseUuid () Void
clubsTrainingPhasesR = mkResourceReader { R.create = Just create
                                        , R.get = Just get
                                        , R.list = const list
                                        , R.name = "training-phases"
                                        , R.remove = Just remove
                                        , R.schema = withListing () $ unnamedSingleRead id
                                        , R.update = Just update }
  where
    create :: Handler (ReaderT ClubUuid App)
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishTrainingPhase -> ExceptT $ do
        clubUuid <- ask
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- lift $ runSql $ conflictInsert $
            TrainingPhase { trainingPhaseUuid = uuid
                          , trainingPhaseName = publishTrainingPhaseName publishTrainingPhase
                          , trainingPhaseCreated = now
                          , trainingPhaseClubUuid = clubUuid }
        case result of
            Right _ -> do
                mbTrainingPhase <- lift $ runSql $ getTrainingPhase clubUuid $ Right uuid
                case mbTrainingPhase of
                    Just trainingPhase -> return $ Right trainingPhase
                    Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    get :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- lift ask
        mbTrainingPhase <- lift $ lift $ runSql $ getTrainingPhase clubUuid $ Right uuid
        case mbTrainingPhase of
            Just trainingPhase -> return $ Right trainingPhase
            Nothing -> return $ Left NotFound
    list :: ListHandler (ReaderT ClubUuid App)
    list = mkListing jsonO $ \_ -> lift $ do -- TODO: rangeOffset, rangeCount
        uuid <- ask
        lift $ runSql $ getTrainingPhases uuid
    remove :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        clubUuid <- ask
        mbTrainingPhase <- lift $ lift $ runSql $ getTrainingPhase clubUuid $ Right uuid
        case mbTrainingPhase of
            Just _ -> do
                lift $ lift $ runSql $ runQuery $ E.deleteKey $ TrainingPhaseKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    update :: Handler (ReaderT TrainingPhaseUuid (ReaderT ClubUuid App))
    update = mkInputHandler (jsonI . jsonO) $ \publishTrainingPhase -> ExceptT $ do
        uuid <- ask
        clubUuid <- lift ask
        mbTrainingPhase <- lift $ lift $ runSql $ getTrainingPhase clubUuid $ Right uuid
        case mbTrainingPhase of
            Just _ -> do
                lift $ lift $ runSql $ do
                    runQuery $ E.update $ \trainingPhase -> do
                        E.set trainingPhase [TrainingPhaseName E.=. E.val (publishTrainingPhaseName publishTrainingPhase)]
                        E.where_ (trainingPhase E.^. TrainingPhaseUuid E.==. E.val uuid)
                    mbTrainingPhase <- getTrainingPhase clubUuid $ Right uuid
                    case mbTrainingPhase of
                        Just trainingPhase -> return $ Right trainingPhase
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound

clubsVideosR :: Resource (ReaderT ClubUuid App) (ReaderT VideoUuid (ReaderT ClubUuid App)) VideoUuid VideoListAccessor VideoStatics
clubsVideosR = mkResourceReader { R.actions = [("upload", upload)]
                                , R.create = Just create
                                , R.get = Just get
                                , R.list = list
                                , R.name = "videos"
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
                                , R.statics = statics
                                }
  where
    create :: Handler (ReaderT ClubUuid App)
    create = mkInputHandler (jsonI . jsonO . jsonE) $ \publishVideo -> ExceptT $ do
        clubUuid <- ask
        uuid <- liftIO nextRandom
        now <- liftIO getCurrentTime
        result <- lift $ runSql $ conflictInsert $
                      Video { videoUuid = uuid
                            , videoTrainingPhaseUuid = publishVideoTrainingPhaseUuid publishVideo
                            , videoMemberUuid = publishVideoMemberUuid publishVideo
                            , videoStatus = Empty
                            , videoCreated = now
                            , videoPublished = Nothing
                            , videoClubUuid = clubUuid }
        case result of
            Right _ -> do
               mbVideo <- lift $ runSql $ getVideo uuid
               case mbVideo of
                   Just video -> return $ Right video
                   Nothing -> return $ Left NotFound
            Left e -> return $ Left e
    download :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    download = mkIdHandler fileO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case videoStatus <$> mbVideo of
            Just Complete -> do
                file <- liftIO $ BL.readFile $ "videos/" ++ (toString uuid) ++ ".webm"
                return $ Right (file, "", False) -- TODO (also in "poster")
            _ -> return $ Left NotFound
    get :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    get = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case mbVideo of
            Just video -> return $ Right video
            Nothing -> return $ Left NotFound
    list :: VideoListAccessor -> ListHandler (ReaderT ClubUuid App)
    list accessor = mkListing jsonO $ \_ -> lift $ do -- TODO: rangeOffset, rangeCount
        uuid <- ask
        lift $ runSql $ getVideos uuid accessor
    poster :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    poster = mkIdHandler fileO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case videoStatus <$> mbVideo of
            Just Complete -> do
                file <- liftIO $ BL.readFile $ "videos/" ++ (toString uuid) ++ ".jpeg"
                return $ Right (file, "", False) -- TODO (also in "download")
            _ -> return $ Left NotFound
    remove :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    remove = mkIdHandler jsonO $ \() uuid -> ExceptT $ do
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case mbVideo of
            Just _ -> do
                lift $ lift $ runSql $ runQuery $ E.deleteKey $ VideoKey uuid
                return $ Right ()
            Nothing -> return $ Left NotFound
    statics :: VideoStatics -> Handler (ReaderT ClubUuid App)
    statics MemberAndTrainingPhase = let memberUuidParam = Param ["memberUuid"] (Right . fromJust . fromString . fromJust . head)
                                         trainingPhaseParam = Param ["trainingPhaseUuid"] (Right . fromJust . fromString . fromJust . head)
                                         params = memberUuidParam `TwoParams` trainingPhaseParam
                                     in mkHandler (jsonO . mkPar params) $ \env -> ExceptT $ do
        let (memberUuid, trainingPhaseUuid) = param env
        uuid <- ask
        videos <- lift $ runSql $ getVideos uuid (VideosByMemberAndTrainingPhase memberUuid trainingPhaseUuid)
        return $ Right videos
    statics TeamAndTrainingPhase = let teamUuidParam = Param ["teamUuid"] (Right . fromJust . fromString . fromJust . head)
                                       trainingPhaseParam = Param ["trainingPhaseUuid"] (Right . fromJust . fromString . fromJust . head)
                                       params = teamUuidParam `TwoParams` trainingPhaseParam
                                   in mkHandler (jsonO . mkPar params) $ \env -> ExceptT $ do
        let (teamUuid, trainingPhaseUuid) = param env
        uuid <- ask
        videos <- lift $ runSql $ getVideos uuid (VideosByTeamAndTrainingPhase teamUuid trainingPhaseUuid)
        return $ Right videos
    update :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    update = mkInputHandler (jsonI . jsonO) $ \publishVideo -> ExceptT $ do
        uuid <- ask
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case mbVideo of
            Just _ -> do
                lift $ lift $ runSql $ do
                    runQuery $ E.update $ \video -> do
                        E.set video [ VideoMemberUuid E.=. E.val (publishVideoMemberUuid publishVideo)
                                    , VideoTrainingPhaseUuid E.=. E.val (publishVideoTrainingPhaseUuid publishVideo) ]
                        E.where_ (video E.^. VideoUuid E.==. E.val uuid)
                    mbVideo <- getVideo uuid
                    case mbVideo of
                        Just video -> return $ Right video
                        Nothing -> return $ Left NotFound
            Nothing -> return $ Left NotFound
    upload :: Handler (ReaderT VideoUuid (ReaderT ClubUuid App))
    upload = mkInputHandler (fileI . stringO . jsonE) $ \bytes -> ExceptT $ do
        uuid <- ask
        mbVideo <- lift $ lift $ runSql $ getVideo uuid
        case mbVideo of
            Just video -> case videoStatus video of
                Empty -> do
                    now <- liftIO getCurrentTime
                    liftIO $ BL.writeFile ("upload/" ++ toString uuid) bytes
                    lift $ lift $ runSql $ runQuery $ E.update $ \video -> do
                        E.set video [VideoStatus E.=. E.val Processing]
                        E.where_ (video E.^. VideoUuid E.==. E.val uuid)
                    return $ Right ("" :: String)
                _ -> return $ Left $ CustomReason $ DomainReason Conflict
            Nothing -> return $ Left NotFound

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]
