module Utilities where

import Control.Monad.Reader
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
