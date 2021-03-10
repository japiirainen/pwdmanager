{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import qualified Crypto.Random as CR
import CryptoHash
import Data.Function ((&))
import qualified Database.SQLite.Simple as SQL
import Lib
import Polysemy (Members, Sem)
import qualified Polysemy as P
import qualified Polysemy.Input as PI
import Polysemy.KVStore (KVStore (..))
import qualified Polysemy.State as PS
import Types (Password, PasswordHash, Username)

runAllEffects ::
  CR.DRG gen =>
  gen ->
  SQL.Connection ->
  (forall r. Members [CryptoHash, KVStore Username PasswordHash] r => Sem r a) ->
  IO a
runAllEffects drg conn program =
  program
    & runCryptoHashAsState
    & runKVStoreAsSQLite
    & PI.runInputConst conn
    & PS.evalState drg
    & P.runM

dbFile :: FilePath
dbFile = "sqlite/pwdmanager.db"

-- | effects returning basic IO so can be run in main
withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS passwords (username TEXT PRIMARY KEY, hash TEXT)"
  f conn

runAddUser :: SQL.Connection -> Username -> Password -> IO ()
runAddUser conn uname pwd = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn (addUser uname pwd)

runValidatePassword :: SQL.Connection -> Username -> Password -> IO Bool
runValidatePassword conn uname pwd = do
  drg <- CR.getSystemDRG
  runAllEffects drg conn (validatePassword uname pwd)

main :: IO ()
main =
  withPasswordDBConnection $ \conn -> do
    putStrLn "Adding a username and password to the store"
    runAddUser conn "testtest" "123123"
    putStr "Validating a good password: "
    runValidatePassword conn "testtest" "123123" >>= printResult
    putStr "Validating a bad password: "
    runValidatePassword conn "testtest" "321321" >>= printResult
  where
    printResult True = putStrLn "Accepted"
    printResult False = putStrLn "Rejected"
