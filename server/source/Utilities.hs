module Utilities where

import Control.Monad.Reader
import Data.Maybe
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
    result <- runQuery $ E.insertUnique e
    return $ case result of
        Just _ -> Right e
        Nothing -> Left $ CustomReason $ DomainReason Conflict

getMembers :: Maybe TeamUuid -> ReaderT ConnectionPool IO [Member]
getMembers mbTeamUuid = do
    members <- runQuery $ E.select $ E.from $ \member -> do
        case mbTeamUuid of
            Just teamUuid -> E.where_ (member E.^. MemberTeamUuid E.==. E.val (Just teamUuid))
            Nothing -> return ()
        E.orderBy [E.asc $ member E.^.MemberName]
        return member
    return (map entityVal members)

-- getMember :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Member UUID -> m (WithVideoUuids Member)
getMember memberOrUuid = do
    (uuid, mbMember) <- case memberOrUuid of
        Left member -> return (memberUuid member, Just member)
        Right uuid -> do
            mbMember <- runQuery $ E.get $ MemberKey uuid
            return (uuid, mbMember)
    case mbMember of
        Just member -> do
            videos <- getVideos (VideosByMember uuid)
            return $ Just $ WithVideoUuids $ WithField (map videoUuid videos) member
        Nothing -> return Nothing

getTeams :: ReaderT ConnectionPool IO [WithMemberUuids Team]
getTeams = do
    teams <- runQuery $ E.select $ E.from $ \team -> do
        E.orderBy [E.asc $ team E.^.TeamName]
        return team
    mbTeams <- forM teams $ getTeamWithoutVideoUuids . Left . entityVal
    return $ catMaybes mbTeams

-- Get team with video UUIDs
-- getTeam :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> Either Team UUID -> m (WithVideoUuids (WithMemberUuids Team))
getTeam teamOrUuid = do
    (uuid, mbTeam) <- case teamOrUuid of
        Left team -> return (teamUuid team, Just team)
        Right uuid -> do
            mbTeam <- runQuery $ E.get $ TeamKey uuid
            return (uuid, mbTeam)
    case mbTeam of
        Just team -> do
            members <- getMembers (Just uuid)
            videos <- getVideos (VideosByTeam uuid)
            return $ Just $
                WithVideoUuids $ WithField (map videoUuid videos) $
                WithMemberUuids $ WithField (map memberUuid members) team
        Nothing -> return Nothing

-- Get team without VideoUuids
getTeamWithoutVideoUuids teamOrUuid = do
    (uuid, mbTeam) <- case teamOrUuid of
        Left team -> return (teamUuid team, Just team)
        Right uuid -> do
            mbTeam <- runQuery $ E.get $ TeamKey uuid
            return (uuid, mbTeam)
    case mbTeam of
        Just team -> do
            members <- getMembers (Just uuid)
            videos <- getVideos (VideosByTeam uuid)
            return $ Just $
                WithMemberUuids $ WithField (map memberUuid members) team
        Nothing -> return Nothing

getTrainingPhases :: ReaderT ConnectionPool IO [WithVideoUuid TrainingPhase]
getTrainingPhases = do
    trainingPhases <- runQuery $ E.select $ E.from $ \trainingPhase -> do
        E.orderBy [E.asc $ trainingPhase E.^.TrainingPhaseName]
        return trainingPhase
    mbTrainingPhases <- forM trainingPhases $ getTrainingPhase . Left . entityVal
    return $ catMaybes mbTrainingPhases

getTrainingPhase :: Either TrainingPhase TrainingPhaseUuid -> ReaderT ConnectionPool IO (Maybe (WithVideoUuid TrainingPhase))
getTrainingPhase trainingPhaseOrUuid = do
    (uuid, mbTrainingPhase) <- case trainingPhaseOrUuid of
        Left trainingPhase -> return (trainingPhaseUuid trainingPhase, Just trainingPhase)
        Right uuid -> do
            mbTrainingPhase <- runQuery $ E.get $ TrainingPhaseKey uuid
            return (uuid, mbTrainingPhase)
    case mbTrainingPhase of
        Just trainingPhase -> do
            video <- getInstructionalVideo uuid
            return $ Just $ WithVideoUuid $ WithField (fmap videoUuid video) trainingPhase
        Nothing -> return Nothing

-- getVideos :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> VideoListAccessor -> m [Video]
getVideos accessor = do
    videos <- runQuery $ E.select $ E.from $ \video -> do
        E.where_ (video E.^. VideoStatus E.==. E.val Complete)
        case accessor of
            AllVideos -> return ()
            InstructionalVideos -> E.where_ $ E.isNothing $ video E.^.VideoMemberUuid
            NonInstructionalVideos -> E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
            VideosByMember memberUuid -> do
                E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
                E.where_ $ video E.^. VideoMemberUuid E.==. E.val (Just memberUuid)
            VideosByMemberAndTrainingPhase memberUuid trainingPhaseUuid -> do
                E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
                E.where_ $ video E.^. VideoMemberUuid E.==. E.val (Just memberUuid)
                E.where_ $ video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid
            VideosByTeam teamUuid -> E.where_ $ E.exists . E.from $ \member -> do
                E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
                E.where_ $ member E.^. MemberTeamUuid E.==. E.val (Just teamUuid)
                E.where_ $ video E.^. VideoMemberUuid E.==. E.just (member E.^. MemberUuid)
            VideosByTeamAndTrainingPhase teamUuid trainingPhaseUuid -> do
                E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
                E.where_ $ E.exists . E.from $ \member -> do
                    E.where_ $ member E.^. MemberTeamUuid E.==. E.val (Just teamUuid)
                    E.where_ $ video E.^. VideoMemberUuid E.==. E.just (member E.^. MemberUuid)
                E.where_ $ video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid
            VideosByTrainingPhase trainingPhaseUuid -> do
                E.where_ $ E.not_ $ E.isNothing $ video E.^.VideoMemberUuid
                E.where_ $ video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid
        E.orderBy [E.desc $ video E.^. VideoPublished]
        return video
    return $ map entityVal videos

getInstructionalVideo trainingPhaseUuid = do
    videos <- runQuery $ E.select $ E.from $ \video -> do
        E.where_ (video E.^. VideoStatus E.==. E.val Complete)
        E.where_ (video E.^. VideoTrainingPhaseUuid E.==. E.val trainingPhaseUuid)
        E.orderBy [E.desc $ video E.^. VideoPublished]
        return video
    case videos of
        [] -> return Nothing
        v:_ -> return (Just (entityVal v))

-- getVideo :: (MonadBaseControl IO m, MonadReader (Pool SqlBackend) m, MonadIO m) => UUID -> m Video
getVideo uuid = do
    video <- runQuery $ E.get $ VideoKey uuid
    return video
