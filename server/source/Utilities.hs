module Utilities where

import Control.Monad.Reader
import Data.Maybe
import Database.Persist
import Database.Persist.Sql
import Lambdatrade hiding (Conflict)
import Rest

import qualified Database.Esqueleto as E

import Other
import Types

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

-- runQuery :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m) => SqlPersistT m b -> m b
runQuery a = ask >>= runSqlPool a

-- conflictInsert :: (MonadBaseControl IO m, PersistEntity b, MonadReader (Pool SqlBackend) m, MonadIO m, (~) * (PersistEntityBackend b) SqlBackend) => b -> m (Either (Reason AppError) b)
conflictInsert e = do
    result <- runQuery (insertUnique e)
    return $ case result of
        Just _ -> Right e
        Nothing -> Left (CustomReason (DomainReason Conflict))

-- m0 ambigous if commented
getClubs :: ReaderT ConnectionPool IO [WithMemberUuids (WithTeamUuids (WithTrainingPhaseUuids (WithVideoUuids Club)))]
getClubs = do
    clubEntities <- runQuery $ selectList [] [Asc ClubName]
    mbClubs <- forM clubEntities $ getClub . Left . entityVal
    return $ catMaybes mbClubs

-- getClub :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => Either Club UUID -> m (WithMemberUuids (WithTeamUuids (WithTrainingPhaseUuids (WithVideoUuids Club))))
getClub value = do
    (uuid, mbClub) <- case value of
        Left club -> return (clubUuid club, Just club)
        Right uuid -> do
            mbClub <- runQuery $ Database.Persist.get $ ClubKey uuid
            return (uuid, mbClub)
    case mbClub of
        Just club -> do
            members <- getMembers uuid Nothing
            teams <- getTeams uuid
            trainingPhases <- getTrainingPhases uuid
            videos <- getVideos uuid AllVideos
            return $ Just $
                WithMemberUuids $ WithField (map (memberUuid . withFieldBase . unWithVideoUuids) members) $
                WithTeamUuids $ WithField (map (teamUuid . withFieldBase . unWithMemberUuids . withFieldBase . unWithVideoUuids) teams) $
                WithTrainingPhaseUuids $ WithField (map (trainingPhaseUuid . withFieldBase . unWithVideoUuids) trainingPhases) $
                WithVideoUuids $ WithField (map videoUuid videos) club
        Nothing -> return Nothing

-- getMembers :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Maybe UUID -> m [WithVideoUuids Member]
getMembers clubUuid mbTeamUuid = do
    members <- runQuery $ flip selectList [Asc MemberName] $ case mbTeamUuid of
        Just teamUuid -> [MemberClubUuid ==. clubUuid, MemberTeamUuid ==. Just teamUuid]
        Nothing -> [MemberClubUuid ==. clubUuid]
    mbMembers <- forM members $ getMember clubUuid . Left . entityVal
    return $ catMaybes mbMembers

-- getMember :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Member UUID -> m (WithVideoUuids Member)
getMember clubUuid memberOrUuid = do
    (uuid, mbMember) <- case memberOrUuid of
        Left member -> return (memberUuid member, Just member)
        Right uuid -> do
            mbMember <- runQuery $ Database.Persist.get $ MemberKey uuid
            return (uuid, mbMember)
    case mbMember of
        Just member -> do
            videos <- getVideos clubUuid (VideosByMember uuid)
            return $ Just $ WithVideoUuids $ WithField (map videoUuid videos) member
        Nothing -> return Nothing

-- getTeams :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m [WithVideoUuids (WithMemberUuids Team)]
getTeams clubUuid = do
    teams <- runQuery $ selectList [TeamClubUuid ==. clubUuid] [Asc TeamName]
    mbTeams <- forM teams $ getTeam clubUuid . Left . entityVal
    return $ catMaybes mbTeams

-- getTeam :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Team UUID -> m (WithVideoUuids (WithMemberUuids Team))
getTeam clubUuid teamOrUuid = do
    (uuid, mbTeam) <- case teamOrUuid of
        Left team -> return (teamUuid team, Just team)
        Right uuid -> do
            mbTeam <- runQuery $ Database.Persist.get $ TeamKey uuid
            return (uuid, mbTeam)
    case mbTeam of
        Just team -> do
            members <- getMembers clubUuid (Just uuid)
            videos <- getVideos clubUuid (VideosByTeam uuid)
            return $ Just $
                WithVideoUuids $ WithField (map videoUuid videos) $
                WithMemberUuids $ WithField (map (memberUuid . withFieldBase . unWithVideoUuids) members) team
        Nothing -> return Nothing

-- getTrainingPhases :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m [WithVideoUuids TrainingPhase]
getTrainingPhases clubUuid = do
    trainingPhases <- runQuery $ selectList [TrainingPhaseClubUuid ==. clubUuid] [Asc TrainingPhaseName]
    mbTrainingPhases <- forM trainingPhases $ getTrainingPhase clubUuid . Left . entityVal
    return $ catMaybes mbTrainingPhases

-- getTrainingPhase :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either TrainingPhase UUID -> m (WithVideoUuids TrainingPhase)
getTrainingPhase clubUuid trainingPhaseOrUuid = do
    (uuid, mbTrainingPhase) <- case trainingPhaseOrUuid of
        Left trainingPhase -> return (trainingPhaseUuid trainingPhase, Just trainingPhase)
        Right uuid -> do
            mbTrainingPhase <- runQuery $ Database.Persist.get $ TrainingPhaseKey uuid
            return (uuid, mbTrainingPhase)
    case mbTrainingPhase of
        Just trainingPhase -> do
            videos <- getVideos clubUuid (VideosByTrainingPhase uuid)
            return $ Just $ WithVideoUuids $ WithField (map videoUuid videos) trainingPhase
        Nothing -> return Nothing

-- getVideos :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> VideoListAccessor -> m [Video]
getVideos clubUuid accessor = do
    videos <- runQuery $ E.select $ E.from $ \video -> do
        E.where_ (video E.^. VideoStatus E.==. E.val Complete)
        E.where_ (video E.^. VideoClubUuid E.==. E.val clubUuid)
        case accessor of
            AllVideos -> return ()
            InstructionalVideos -> E.where_ $ E.isNothing $ video E.^.VideoMemberUuid
            VideosByMember memberUuid -> E.where_ $ video E.^. VideoMemberUuid E.==. E.val (Just memberUuid)
            VideosByTeam teamUuid -> E.where_ $ E.exists . E.from $ \member -> do
                E.where_ $ member E.^. MemberTeamUuid E.==. E.val (Just teamUuid)
                E.where_ $ video E.^. VideoMemberUuid E.==. E.just (member E.^. MemberUuid)
            VideosByTrainingPhase trainingPhaseUuid -> E.where_ $ video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid
        E.orderBy [E.desc $ video E.^. VideoPublished]
        return video
    return $ map entityVal videos

-- getVideo :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m Video
getVideo uuid = do
    video <- runQuery $ Database.Persist.get $ VideoKey uuid
    return video
