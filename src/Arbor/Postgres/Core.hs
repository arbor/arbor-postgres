{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Postgres.Core where

import Arbor.Postgres.Config      (PostgresConfig)
import Arbor.Postgres.Password
import Control.Lens
import Data.Generics.Product.Any
import Data.Monoid
import Database.PostgreSQL.Simple (Connection)

import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PGS

connectPostgres :: PostgresConfig -> IO Connection
connectPostgres postgresConfig = do
  let host              = postgresConfig ^. the @"host"
  let dbname            = postgresConfig ^. the @"database"
  let user              = postgresConfig ^. the @"user"
  let Password password = postgresConfig ^. the @"password"
  PGS.connectPostgreSQL $ T.encodeUtf8 $ mempty
    <> "host='"     <> host     <> "' "
    <> "dbname='"   <> dbname   <> "' "
    <> "user='"     <> user     <> "' "
    <> "password='" <> password <> "'"
