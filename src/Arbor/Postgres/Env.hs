{-# LANGUAGE DeriveGeneric #-}

module Arbor.Postgres.Env where

import GHC.Generics
import Network.URI

import qualified Database.PostgreSQL.Simple as PGS

data PostgresEnv = PostgresEnv
  { conn       :: PGS.Connection
  , connString :: URI
  } deriving Generic
