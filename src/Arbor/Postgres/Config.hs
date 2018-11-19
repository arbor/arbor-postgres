{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Arbor.Postgres.Config
  ( optsPostgresConfig
  , optsConnectInfo
  , configToConnectInfo
  , connectInfoToConfig
  , PostgresConfig (..)
  )
where

import Arbor.Postgres.Password
import Data.Semigroup             ((<>))
import Data.String                (IsString)
import Data.Text                  (Text, pack, unpack)
import Database.PostgreSQL.Simple as PG
import GHC.Generics
import Options.Applicative

data PostgresConfig = PostgresConfig
  { host     :: Text
  , database :: Text
  , user     :: Text
  , password :: Maybe Password
  } deriving (Eq, Show, Generic)

hostOption :: IsString s => String -> Parser s
hostOption prefix = strOption
  (  long (prefix <> "-db-host")
  <> metavar "DB_HOST"
  <> help "The postgres hostname"
  )

databaseOption :: IsString s => String -> Parser s
databaseOption prefix = strOption
  (  long (prefix <> "-db-name")
  <> metavar "DB_NAME"
  <> help "The postgres db name"
  )

userOption :: IsString s => String -> Parser s
userOption prefix = strOption
  (  long (prefix <> "-db-user")
  <> metavar "DB_USER"
  <> help "The postgres user"
  )

passwordOption :: IsString s => String -> Parser s
passwordOption prefix = strOption
  (  long (prefix <> "-db-password")
  <> metavar "DB_PASSWORD"
  <> help "The postgres password"
  )

optsPostgresConfig :: String -> Parser PostgresConfig
optsPostgresConfig prefix = do
  host     <- hostOption prefix
  database <- databaseOption prefix
  user     <- userOption prefix
  password <- optional $ Password <$> passwordOption prefix
  return PostgresConfig {..}

optsConnectInfo :: String -> Parser PG.ConnectInfo
optsConnectInfo prefix = do
  let connectPort = 5432
  connectHost     <- hostOption prefix
  connectDatabase <- databaseOption prefix
  connectUser     <- userOption prefix
  connectPassword <- passwordOption prefix
  return PG.ConnectInfo {..}

configToConnectInfo :: PostgresConfig  -> PG.ConnectInfo
configToConnectInfo pgc =
  let pass = case password pgc of
              Nothing           -> ""
              Just (Password p) -> p
  in ConnectInfo
    { connectHost = unpack $ host pgc
    , connectDatabase = unpack $ database pgc
    , connectUser = unpack $ user pgc
    , connectPassword = unpack pass
    , connectPort = 5432
    }

connectInfoToConfig :: PG.ConnectInfo -> PostgresConfig
connectInfoToConfig pgc =
  let pass = case connectPassword pgc of
              "" -> Nothing
              p  -> Just . Password . pack $ p
  in PostgresConfig
    { host = pack $ connectHost pgc
    , database = pack $ connectDatabase pgc
    , user = pack $ connectUser pgc
    , password = pass
    }
