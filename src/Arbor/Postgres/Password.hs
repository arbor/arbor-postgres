module Arbor.Postgres.Password where

import Data.Text (Text)

newtype Password = Password Text
  deriving Eq

instance Show Password where
  showsPrec _ _ = ("********" ++)
