{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Arbor.Postgres.Core where

import Control.Exception         (catch, throw)
import Control.Lens
import Control.Monad             (void)
import Data.ByteString
import Data.Generics.Product.Any
import Data.Int
import Data.Monoid               ((<>))
import Data.String
import Network.URI

import qualified Arbor.Postgres.Config      as Z
import qualified Arbor.Postgres.Env         as E
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PGS

parseConfig :: Z.PostgresConfig -> ByteString
parseConfig = PGS.postgreSQLConnectionString . Z.configToConnectInfo

-- because we need a double quoted, not single quoted string,
-- we need to explicitly not use sql interpolation here.
createDatabaseStatement :: T.Text -> PGS.Query
createDatabaseStatement db = fromString . T.unpack $ "CREATE DATABASE \"" <> db <> "\""

createDatabase :: Z.PostgresConfig -> IO ()
createDatabase postgresConfig = do
  conn <- PGS.connectPostgreSQL $ parseConfig $ postgresConfig { Z.database = "postgres" }
  let q = createDatabaseStatement (postgresConfig ^. the @"database")
  void $ PGS.execute_ conn q `catch` duplicateDatabase

-- https://www.postgresql.org/docs/8.2/static/errcodes-appendix.html
-- 42P04	DUPLICATE DATABASE	duplicate_database
duplicateDatabase :: PGS.SqlError -> IO Int64
duplicateDatabase e =
  if PGS.sqlState e == "42P04"
    then return 0
    else throw e

connectPostgres :: Z.PostgresConfig -> IO E.PostgresEnv
connectPostgres postgresConfig = do
  conn <- PGS.connectPostgreSQL $ parseConfig postgresConfig
  return $ E.PostgresEnv conn (mkConnectionString postgresConfig)

newtype Table = Table
  { table :: T.Text }
  deriving (IsString, Show)

mkConnectionString :: Z.PostgresConfig -> URI
mkConnectionString config = do
  let host = config ^. the @"host" & T.unpack
  let dbname = config ^. the @"database" & T.unpack
  let auth = pure $ URIAuth "" host ":5432"
  let q = ""
  let frag = ""
  URI "postgresql:" auth ("/" <> dbname) q frag

mkResourceURI :: URI -> Table -> [(T.Text, T.Text)] -> URI
mkResourceURI uri (Table tbl) kvs = do
  let q = "?" <> T.intercalate "&" (uncurry (\k v -> k <> "=" <> v) <$> (("table", tbl) : kvs)) & T.unpack
  uri { uriQuery = q }
