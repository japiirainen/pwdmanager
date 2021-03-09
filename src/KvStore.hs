module KvStore where

import qualified Crypto.Random as CR
import CryptoHash
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy as P
import qualified Polysemy.Input as PI
import Polysemy.KVStore (KVStore (..))
import qualified Polysemy.State as PS
import Types

runKVStoreAsSQLite ::
  Member (Embed IO) r =>
  Sem (KVStore Username PasswordHash : r) a ->
  Sem (PI.Input SQL.Connection : r) a
runKVStoreAsSQLite = P.reinterpret $ \case
  LookupKV username -> do
    conn <- PI.input
    hashes <-
      P.embed $
        SQL.queryNamed
          conn
          "SELECT hash FROM passwords WHERE username = :username"
          [":username" := username]
    return (SQL.fromOnly <$> listToMaybe hashes)
  UpdateKV username maybeHash -> do
    let (query, params) =
          case maybeHash of
            Just hash ->
              ( "INSERT INTO passwords (username, hash) VALUES (:username, :hash) "
                  <> "ON CONFLICT (username) DO UPDATE SET hash = excluded.hash",
                [":username" := username, ":hash" := hash]
              )
            Nothing ->
              ( "DELETE FROM passwords WHERE username = :username",
                [":username" := username]
              )
    conn <- PI.input
    P.embed $ SQL.executeNamed conn query params
