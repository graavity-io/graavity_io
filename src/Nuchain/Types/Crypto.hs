{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nuchain.Types.Crypto
  ( KeyPair(..), kpPublicKey, kpPrivateKey
  , KeySet(..), ksCluster
  , Signer(..), siPubKey, siAddress, siScheme
  , sign
  , Ed25519.exportPublic, Ed25519.exportPrivate
  , valid ) where

import Control.DeepSeq
import Control.Lens (makeLenses)

import qualified Crypto.Ed25519.Pure as Ed25519
import qualified Data.ByteString.Char8 as Char8

import Data.Aeson
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as Short
import Data.Serialize (Serialize(..))
import Data.Text (Text)

import GHC.Generics

import qualified Pact.Types.Hash as P
import qualified Pact.Types.Scheme as P
import Pact.Types.Util (lensyParseJSON, lensyToJSON, toB16Text)
import qualified Pact.Types.Util as P

import Nuchain.Types.Base (Alias)

instance Eq Ed25519.Signature where
  (==) (Ed25519.Sig s1) (Ed25519.Sig s2) = s1 == s2

instance Eq Ed25519.PublicKey where
  (==) pk1 pk2 = (show pk1) == (show pk2)

instance Eq Ed25519.PrivateKey where
  (==) pk1 pk2 = (show pk1) == (show pk2)

instance ToJSON Ed25519.PublicKey where
  toJSON = String . P.toB16Text . Ed25519.exportPublic
instance FromJSON Ed25519.PublicKey where
  parseJSON = withText "Ed25519.PublicKey" P.parseText
  {-# INLINE parseJSON #-}
instance P.ParseText Ed25519.PublicKey where
  parseText s = do
    s' <- P.parseB16Text s
    case Ed25519.importPublic s' of
      Just v -> return v
      Nothing -> fail ("Public key import failed: " ++ show s)
  {-# INLINE parseText #-}

instance Serialize Ed25519.PublicKey where
  put pk1 = put $ toB16Text (BSL.toStrict $ encode pk1)
  get = do
    x <- get
    case P.fromText' x of
      Right v -> return v
      Left err -> undefined

----------------------------------------------------------------------------------------------------
instance ToJSON Ed25519.PrivateKey where
  toJSON = String . P.toB16Text . Ed25519.exportPrivate
instance FromJSON Ed25519.PrivateKey where
  parseJSON = withText "Ed25519.PrivateKey" P.parseText
  {-# INLINE parseJSON #-}
instance P.ParseText Ed25519.PrivateKey where
  parseText s = do
    s' <- P.parseB16Text s
    case Ed25519.importPrivate s' of
      Just v -> return v
      Nothing -> fail ("Public key import failed: " ++ show s)
  {-# INLINE parseText #-}

----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
data KeyPair = KeyPair
  { _kpPublicKey :: Ed25519.PublicKey
  , _kpPrivateKey :: Ed25519.PrivateKey
  } deriving (Show, Eq, Generic)
makeLenses ''KeyPair

instance ToJSON KeyPair where
  toJSON (KeyPair p s) =
    object [ "publicKey" .= toB16Text (Ed25519.exportPublic p)
           , "privateKey" .= toB16Text (Ed25519.exportPrivate s) ]

instance FromJSON KeyPair where
    parseJSON = withObject "KeyPair" $ \v -> do
      p <- v .: "publicKey"
      case Ed25519.importPublic (Char8.pack p) of
        Just pkey -> do
          pr <- v .: "privateKey"
          case Ed25519.importPrivate (Char8.pack pr) of
            Just prkey -> return $ KeyPair pkey prkey
            _ -> fail "unable to load privateKey"
        _ -> fail "unable to load privateKey"

----------------------------------------------------------------------------------------------------
data Signer = Signer
  { _siScheme :: P.PPKScheme
  , _siPubKey :: !Ed25519.PublicKey
  , _siAddress :: Text }
  deriving (Show, Eq, Generic)

instance Serialize Signer
instance ToJSON Signer where toJSON = lensyToJSON 3
instance FromJSON Signer where parseJSON = lensyParseJSON 3

-- Nothing really to do for Signer as NFData, but to convice the compiler:
instance NFData Signer where rnf Signer{}  = ()

makeLenses ''Signer

----------------------------------------------------------------------------------------------------
data KeySet = KeySet
  { _ksCluster :: !(Map Alias Ed25519.PublicKey)
  } deriving Eq
makeLenses ''KeySet

instance Default KeySet where
  def = KeySet Map.empty

----------------------------------------------------------------------------------------------------
valid :: P.Hash -> Ed25519.PublicKey -> Ed25519.Signature -> Bool
valid (P.Hash msg) pub sig = Ed25519.valid (Short.fromShort msg) pub sig

----------------------------------------------------------------------------------------------------
sign :: P.Hash -> Ed25519.PrivateKey -> Ed25519.PublicKey -> Ed25519.Signature
sign (P.Hash msg) priv pub = Ed25519.sign (Short.fromShort msg) priv pub

