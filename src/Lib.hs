module Lib
  ( addUser,
    validatePassword,
    module Types,
    runKVStoreAsSQLite,
    module CryptoHash,
  )
where

import CryptoHash
import KvStore
import Polysemy (Members, Sem)
import Polysemy.KVStore (KVStore, lookupKV, writeKV)
import Types

addUser ::
  Members [CryptoHash, KVStore Username PasswordHash] r =>
  Username ->
  Password ->
  Sem r ()
addUser uname pwd = do
  hashedPwd <- makeHash pwd
  writeKV uname hashedPwd

validatePassword ::
  Members [CryptoHash, KVStore Username PasswordHash] r =>
  Username ->
  Password ->
  Sem r Bool
validatePassword uname pwd = do
  hashInStore <- lookupKV uname
  case hashInStore of
    Just v -> validateHash pwd v
    Nothing -> return False