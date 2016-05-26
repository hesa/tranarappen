{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.Time.Clock
import Data.Typeable
import Data.UUID
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import Generics.Generic.Aeson
import GHC.Generics
import Lambdatrade hiding (Conflict)
import Rest

import Data.Text (Text)

import qualified Control.Monad.Catch as C
import qualified Data.JSON.Schema as Schema

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
newtype WithVideoUuid a = WithVideoUuid { unWithVideoUuid :: (WithField "videoUuid" (Maybe UUID) a) } deriving (FromJSON, Schema.JSONSchema, ToJSON, Typeable)

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

type ClubUuid = UUID
type MemberUuid = UUID
type TeamUuid = UUID
type TrainingPhaseUuid = UUID
type VideoUuid = UUID

data ClubComposite = ClubComposite { clubCompositeUuid :: !ClubUuid
                                   , clubCompositeName :: !Text
                                   , clubCompositeCreated :: !UTCTime
                                   , clubCompositeMembers :: ![Member]
                                   , clubCompositeTeams :: ![WithMemberUuids Team]
                                   , clubCompositeTrainingPhases :: ![WithVideoUuid TrainingPhase] }

mkGenericJSON [t|ClubComposite|]

data AppError = Conflict deriving Typeable

instance ToResponseCode AppError where
    toResponseCode Conflict = 409

instance Schema.JSONSchema AppError where
    schema _ = Schema.Object []

instance ToJSON AppError where
    toJSON _ = object []

data VideoStatics = MemberAndTrainingPhase | TeamAndTrainingPhase

data VideoListAccessor = AllVideos
                       | InstructionalVideos
                       | NonInstructionalVideos
                       | VideosByMember MemberUuid
                       | VideosByMemberAndTrainingPhase MemberUuid TrainingPhaseUuid
                       | VideosByTeam TeamUuid
                       | VideosByTeamAndTrainingPhase TeamUuid TrainingPhaseUuid
                       | VideosByTrainingPhase TrainingPhaseUuid
