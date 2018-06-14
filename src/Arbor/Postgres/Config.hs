{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Arbor.Postgres.Config where

import Arbor.Postgres.Password
import Data.Semigroup          ((<>))
import Data.Text               (Text)
import GHC.Generics
import Options.Applicative

data PostgresConfig = PostgresConfig
  { host     :: Text
  , database :: Text
  , user     :: Text
  , password :: Password
  } deriving (Eq, Show, Generic)

optsPostgresConfig :: String -> Parser PostgresConfig
optsPostgresConfig prefix = do
  host <- strOption
    (  long (prefix <> "-db-host")
    <> metavar "DB_HOST"
    <> help "The postgres hostname"
    )
  database <- strOption
    (  long (prefix <> "-db-name")
    <> metavar "DB_NAME"
    <> help "The postgres db name"
    )
  user <- strOption
    (  long (prefix <> "-db-user")
    <> metavar "DB_USER"
    <> help "The postgres user"
    )
  password <- Password <$> strOption
    (  long (prefix <> "-db-password")
    <> metavar "DB_PASSWORD"
    <> help "The postgres password"
    )
  return PostgresConfig {..}

