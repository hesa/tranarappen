{-# LANGUAGE TemplateHaskell #-}

module Other where

import Database.Persist.TH

data VideoStatus = Empty | Processing | Complete | Failure deriving (Read, Show)

derivePersistField "VideoStatus"
