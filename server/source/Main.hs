{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import Lambdatrade

import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "schema")

main :: IO ()
main = withPool 10 $ do
    \pool -> liftIO $ do
        runSqlPool (runMigration migrateAll) pool
        putStrLn "Hello Lambdatrade Reference Architecture!"
