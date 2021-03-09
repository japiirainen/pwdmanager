module CryptoHash where

import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Crypto.Random as CR
import Polysemy (Member, Sem)
import qualified Polysemy as P
import qualified Polysemy.Internal as P
import Polysemy.State (State)
import qualified Polysemy.State as PS
import Types

data CryptoHash m a where
  MakeHash :: Password -> CryptoHash m PasswordHash
  ValidateHash :: Password -> PasswordHash -> CryptoHash m Bool

makeHash :: Member CryptoHash r => Password -> Sem r PasswordHash
makeHash x = P.send (MakeHash x :: CryptoHash (Sem r) PasswordHash)

validateHash :: Member CryptoHash r => Password -> PasswordHash -> Sem r Bool
validateHash pwd hash = P.send (ValidateHash pwd hash :: CryptoHash (Sem r) Bool)

-- | implementation
runCryptoHashAsState :: (CR.DRG gen, Member (State gen) r) => Sem (CryptoHash : r) a -> Sem r a
runCryptoHashAsState = P.interpret $ \case
  ValidateHash pwd hash -> return (BCrypt.validatePassword pwd hash)
  MakeHash pwd -> do
    drg <- PS.get
    let (hash, drg') = CR.withDRG drg $ BCrypt.hashPassword 5 pwd
    PS.put drg'
    return hash