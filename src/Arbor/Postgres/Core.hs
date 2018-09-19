{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Postgres.Core where

import Arbor.Postgres.Config     (PostgresConfig)
import Arbor.Postgres.Password
import Control.Lens
import Data.Generics.Product.Any
import Data.Monoid               ((<>))
import Data.String
import Network.URI

import qualified Arbor.Postgres.Env         as E
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PGS

connectPostgres :: PostgresConfig -> IO E.PostgresEnv
connectPostgres postgresConfig = do
  let host     = postgresConfig ^. the @"host"
  let dbname   = postgresConfig ^. the @"database"
  let user     = postgresConfig ^. the @"user"
  let mPassword = postgresConfig ^. the @"password"
  let kvPassword = case mPassword of
        Just (Password password) -> [("password", password)]
        Nothing                  -> []
  let kvs = [("host", host), ("dbname", dbname), ("user", user)] <> kvPassword
  let kvStrings = kvs <&> (\(k, v) -> k <> "='" <> v <> "'")
  conn <- PGS.connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " kvStrings
  return $ E.PostgresEnv conn (mkConnectionString postgresConfig)

newtype Table = Table
  { table :: T.Text }
  deriving (IsString, Show)

mkConnectionString :: PostgresConfig -> URI
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
