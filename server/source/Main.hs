{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Typeable
import Data.UUID
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import Lambdatrade
import Network.Wai.Handler.Warp
import Rest
import Rest.Api
import Rest.Driver.Wai

import Data.Text (Text)

import qualified Data.JSON.Schema as Schema
import qualified Rest.Resource as R

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "schema")

newtype App a = App { unApi :: ReaderT ConnectionPool IO a } deriving (Applicative, Functor, Monad)

instance Schema.JSONSchema Club where
    schema _ = Schema.Object [ Schema.Field "name" True (Schema.Value (Schema.unboundedLength)) ]

deriving instance Typeable Club

runSql :: ReaderT ConnectionPool IO a -> App a
runSql = App

router :: Router App App
router = root -/ route clubsR
  where
    clubsR :: Resource App (ReaderT Club App) Club () Void
    clubsR = mkResourceReader { R.name = "clubs"
                              , R.list = const list
                              , R.schema = withListing () $ named [] }
      where
        list :: ListHandler App
        list = mkListing jsonO $ \_ -> lift $ runSql $
            ask >>= runSqlPool (selectList [] [Asc ClubName]) >>= return . map entityVal

api :: Api App
api = [(mkVersion 0 0 0, Some1 router)]

main :: IO ()
main = withPool 10 $ do
    \pool -> liftIO $ do
        runSqlPool (runMigration migrateAll) pool
        run 3000 (apiToApplication (\(App r) -> runReaderT r pool) api)
