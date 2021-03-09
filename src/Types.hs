module Types where

import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.String
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

newtype Username = Username ByteString
  deriving (Eq, Ord, Show, ToField, IsString)

newtype Password = Password ByteString
  deriving (Semigroup, Eq, Ord, Show, Monoid, ByteArrayAccess, ByteArray, IsString)

newtype PasswordHash = PasswordHash ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray, ToField, FromField)