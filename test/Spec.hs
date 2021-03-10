module Main where

import qualified Crypto.Random as CR
import CryptoHash
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Lib
import Polysemy (Members, Sem)
import qualified Polysemy as P
import Polysemy.KVStore (KVStore (..))
import qualified Polysemy.KVStore as KV
import qualified Polysemy.State as PS
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

runAllEffects ::
  CR.DRG gen =>
  gen ->
  (forall r. Members [CryptoHash, KVStore Username PasswordHash] r => Sem r a) ->
  a
runAllEffects drg program =
  program
    & runCryptoHashAsState
    & KV.runKVStorePurely Map.empty
    & PS.evalState drg
    & P.run
    & snd

main :: IO ()
main = do
  drg <- CR.getSystemDRG

  hspec $
    describe "basic validation" $
      it "password added can be validated" $
        property $ \user pass -> addAndValidate drg (Username user) (Password pass) === True
  where
    addAndValidate :: CR.DRG gen => gen -> Username -> Password -> Bool
    addAndValidate drg user pwd = runAllEffects drg $ do
      addUser user pwd
      validatePassword user pwd
