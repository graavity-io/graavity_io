{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Codec.Winery
import Data.Aeson
import Codec.Winery.Class
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.Default (def)
import Data.Time.Clock
import Data.Time.Format
import Data.Scientific
import Data.Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.List
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.PubKey.Ed25519 as Ed25519

import GHC.Generics
-- Basic imports
import qualified Network.Ethereum
import qualified Network.Web3 as Web3
import qualified Network.Web3.Provider as Web3

-- Eth API support
import qualified Network.Ethereum.Api.Eth   as ETH
import           Network.Ethereum.Api.Types (Quantity, DefaultBlock( Latest ), Call (..), TxReceipt)
import qualified Data.ByteArray.HexString.Internal as BA

-- ENS support
import qualified Network.Ethereum.Ens       as Ens
import qualified Network.Ethereum.Api.Eth as ETH
import Network.Wai.Handler.Warp
import Nuchain.Util.SeedPhrase
import Servant
import Servant.API
import Servant.Server
import System.Environment
import System.Random (randomRIO)

import Data.Digest.Pure.SHA

import qualified Pact.Server.API as P
import qualified Pact.Types.Exp as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.API as P
import qualified Pact.Types.Crypto as P
import Pact.JSON.Encode (Encode, toJsonViaEncode)
import qualified Pact.ApiReq as P

-- RPC imports
import Network.RPC.Curryer.Client
import Network.RPC.Curryer.Server (localHostAddr, ConnectionError)

--- Overlappable instances
--
instance Encode a => ToJSON (P.Command a) where
  toJSON = toJsonViaEncode

instance ToJSON P.RequestKeys where
  toJSON = toJsonViaEncode

instance ToJSON P.RequestKey where
  toJSON = toJsonViaEncode

data Input = Input
  { prevout :: OutPoint
  , prevoutData :: Output
  }
  deriving (Generic, Show, Ord, Eq, FromJSON, ToJSON)

data Output = Output
  { witnessProgramCommitment :: Text
  , value :: Int -- ^ The integral value of the output, in atomic units of currency
  }
  deriving (Generic, Show, Ord, Eq, FromJSON, ToJSON)

data OutPoint = OutPoint
  { txHash :: Text
  , txIndex :: Int
  }
  deriving (Generic, Show, Ord, Eq, FromJSON, ToJSON)

data TxRequest = TxRequest
  { inputs :: [Input]
  , outputs :: [Output]
  , witness :: [String] -- ^ should be raw bytes.
  }
  deriving (Generic, Show, Eq, FromJSON)

data TxResponse = TxResponse
  { resp :: Text
  }
  deriving (Generic, Show, Eq, ToJSON)

data PactRequest = PactRequest
  { cmd :: Text
  , user :: String
  }
  deriving (Generic, Show, Eq, FromJSON)
  -- deriving Serialise via WineryVariant PactRequest

data SCRequest = SCRequest
  { code :: Text --P.Command (P.Payload P.PrivateMeta P.ParsedCode)
  , publicKey :: String
  , key :: Text --P.RequestKeys
  , originalReq :: Text
  , localOrSend :: String -- "LOCAL" | "SEND"
  }
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SCRequest

data UserRequest = UserRequest
  { signedUserPublicKey :: String
  , initialAmount :: Scientific
  , hash :: Text -- { hash: "signature_value_using_ed25519_on_blake256_hash_of_(create-user signerUserPublicKey initialAmount)" }
  }
  deriving (Generic, Show, Eq, FromJSON)
  deriving Serialise via WineryVariant UserRequest

-- instance Encode P.ParsedCode where
--   toJSON parsedCode = toJsonViaEncode

data TransferRequest = TransferRequest
  { userKey :: String
  , to   :: String
  , amount :: Scientific
  , hash :: Text -- { hash: "signature_value_of_ed25519_on_(transfer userKey to amount timestamp)" }
  , timestamp :: String
  }
  deriving (Generic, FromJSON)
  deriving Serialise via WineryVariant TransferRequest

data CreateUserDetails = CreateUserDetails
  { publickKey :: Text
  , privateKey :: Text
  , seedPhrase :: SeedPhrase
  , signature :: Text  -- { hash: "signature_value_of_ed25519_on_(create-user publickKey o)" } 0 -> Initial Amount
  }
  deriving (Generic, ToJSON)

data ImportUserRequest = ImportUserRequest
  { privateKey :: Maybe String
  , seedPhrase :: Maybe SeedPhrase
  }
  deriving (Generic, ToJSON, FromJSON)

data TransferDetailsRequest = TransferDetailsRequest
  { fromKeyPub :: String
  , fromKeyPriv :: String
  , toKeyPub :: String
  , transferAmount :: Scientific
  }
  deriving (Generic, ToJSON, FromJSON)

data TransferDetailsResponse = TransferDetailsResponse
  { signature :: Text  -- { hash: "signature_value_of_ed25519_on_(transfer userKey to amount time)" }
  , time :: String
  }
  deriving (Generic, ToJSON)

type APIs =
  "submit" :> "tx" :> ReqBody '[JSON] TxRequest :> Post '[JSON] TxResponse
  :<|> "local" :> ReqBody '[JSON] PactRequest :> Post '[JSON] P.RequestKey
  :<|> "send" :> ReqBody '[JSON] PactRequest :> Post '[JSON] P.RequestKey
  :<|> "create" :> "user" :> ReqBody '[JSON] UserRequest :> Get '[PlainText] Text
  :<|> "transfer" :> ReqBody '[JSON] TransferRequest :> Get '[JSON] Text
  :<|> "create" :> "user" :> "details" :> Get '[JSON] CreateUserDetails
  :<|> "import" :> "user" :> "details" :> ReqBody '[JSON] ImportUserRequest :> Post '[JSON] CreateUserDetails
  :<|> "transfer" :> "details" :> ReqBody '[JSON] TransferDetailsRequest :> Post '[JSON] TransferDetailsResponse
  :<|> "getHash" :> ReqBody '[JSON] URequest :> Post '[JSON] UResponse
  -- :<|>  "eth" :> "balance" :> ReqBody '[JSON] LinkEthRequest :> Post '[JSON] Text
  -- :<|> "eth" :> "send" :> ReqBody '[JSON] EthTransferRequest :> Post '[JSON] Text

sentinelAPI :: Proxy APIs
sentinelAPI = Proxy

sentinelServer :: Config -> [Connection] -> Server APIs
sentinelServer config conns =
  submitTx :<|> handleSC config conns :<|> deploySC config conns
  :<|> createUser config conns :<|> transfer config conns :<|> createUserDetails
  :<|> importUserDetails :<|> transferDetails :<|> getHash config conns

sentinelApp :: Config -> [Connection] -> Application
sentinelApp config conns = serve sentinelAPI (sentinelServer config conns)

createUserDetails :: Handler CreateUserDetails
createUserDetails = do
  (sp, pri, pub) <- liftIO generateKeys
  let pub' = decodeUtf8 $ B16.encode $ P.exportEd25519PubKey pub
      priv' = decodeUtf8 $ B16.encode $ P.exportEd25519SecretKey pri
      hashedMsg = P.hash $ BC.pack $ "(create-user " <> unpack pub' <> " " <> "0.0" <> ")"
      signed = getSign pub pri hashedMsg
      exported = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed
  return $ CreateUserDetails pub' priv' sp exported

importUserDetails :: ImportUserRequest -> Handler CreateUserDetails
importUserDetails ImportUserRequest{..} = do
  case (privateKey, seedPhrase) of
    (Nothing, Nothing) -> throwError $ err400 { errBody = "No User found!"}
    (_, Just sp) -> do
      pri <- liftIO $ derivePrivateKey sp
      let pub = Ed25519.toPublic pri
      let pub' = decodeUtf8 $ B16.encode $ P.exportEd25519PubKey pub
          pri' = decodeUtf8 $ B16.encode $ P.exportEd25519SecretKey pri
          hashedMsg = P.hash $ BC.pack $ "(create-user " <> unpack pub' <> " " <> "0.0" <> ")"
          signed = getSign pub pri hashedMsg
          exported = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed
      return $ CreateUserDetails pub' pri' sp exported
    (Just pk,_) ->
      case (B16.decode (BC.pack pk)) of
        Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
        Right priv ->
          case P.parseEd25519SecretKey priv of
            Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
            Right pri -> do
              let pub = Ed25519.toPublic pri
              let pub' = decodeUtf8 $ B16.encode $ P.exportEd25519PubKey pub
                  pri' = decodeUtf8 $ B16.encode $ P.exportEd25519SecretKey pri
                  hashedMsg = P.hash $ BC.pack $ "(create-user " <> unpack pub' <> " " <> "0.0" <> ")"
                  signed = getSign pub pri hashedMsg
                  exported = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed
              return $ CreateUserDetails pub' pri' [] exported

transferDetails :: TransferDetailsRequest -> Handler TransferDetailsResponse
transferDetails req@TransferDetailsRequest{..}
  | fromKeyPub == toKeyPub = throwError $ err400 { errBody = "Failure: Source and Destination are the same"}
  | otherwise = do
    ts <- liftIO getCurrentTimestamp
    let hashedMsg = P.hash $ BC.pack $ "(transfer " <> fromKeyPub <> " " <> toKeyPub <> " " <> show transferAmount <> " " <> ts <> ")"
    case (B16.decode (BC.pack fromKeyPub), B16.decode (BC.pack fromKeyPriv)) of
      (Right pub', Right pri') -> do
        case (P.parseEd25519PubKey pub', P.parseEd25519SecretKey pri') of
          (Right pub, Right priv) -> do
            let signed = getSign pub priv hashedMsg
                exported = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed
            return $ TransferDetailsResponse exported ts
          (Left _, Left _) ->
            throwError $ err400 { errBody = "No User found!"}
          _ -> throwError $ err400 { errBody = "No User found!"}
      (Left _, Left _) ->
        throwError $ err400 { errBody = "No User found!"}
      _ -> throwError $ err400 { errBody = "No User found!"}

data URequest = URequest
  { amtCU :: Scientific
  , amtTR :: Scientific
  , userK :: Text
  } deriving (Generic, Show, Eq, FromJSON, Serialise)

data UResponse = UResponse
  { pkey :: Text
  , privKey :: Text
  , signSC :: Text
  , signCU :: Text
  , sId1 :: Int
  , sIndex1 :: Maybe Int
  , signTR :: Text
  , sId2 :: Int
  , sIndex2 :: Maybe Int
  , sId3 :: Int
  , sIndex3 :: Maybe Int
  , cmdText :: Text
  , ts :: String
  , c :: Text
  } deriving (Generic, Show, Eq, ToJSON)

getHash :: Config -> [Connection] -> URequest -> Handler UResponse
getHash config conns URequest{..} = do
  P.DynEd25519KeyPair (a,b) <- liftIO $ P.DynEd25519KeyPair <$> P.genKeyPair
  _ <- liftIO $ print $ decodeUtf8 $ B16.encode $ P.exportEd25519SecretKey b
  (sp, pri1, pub1) <- liftIO generateKeys
  ts <- liftIO $ getCurrentTimestamp
  let pub1' = decodeUtf8 $ B16.encode $ P.exportEd25519PubKey pub1
      pri1' = decodeUtf8 $ B16.encode $ P.exportEd25519SecretKey pri1
      hashedMsg1 = P.hash $ BC.pack $ "(create-user " <> unpack pub1' <> " " <> show amtCU <> ")"
      hashedMsg2 = P.hash $ BC.pack $ "(transfer " <> unpack pub1' <> " " <> unpack userK <> " " <> show amtTR <> " " <> ts <> ")"
      signed1 = getSign pub1 pri1 hashedMsg1
      signed2 = getSign pub1 pri1 hashedMsg2
      t1 = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed1
      s1 = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (unpack pub1'))
      shardId1 = fromInteger $ s1 `rem` 255
      shardIndex1 = Data.List.findIndex (\(x,y) -> shardId1 >= x && shardId1 <= y) (shardRange config)
      t2 = decodeUtf8 $ B16.encode $ P.exportEd25519Signature signed2
      s2 = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (unpack pub1'))
      shardId2 = fromInteger $ s2 `rem` 255
      shardIndex2 = Data.List.findIndex (\(x,y) -> shardId2 >= x && shardId2 <= y) (shardRange config)
      reqKey = P.hash (BC.pack (cmdPayloadTemplate ++ unpack pub1' ++ "\"}]}" ))
      scSign = getSign pub1 pri1 reqKey
      scSignText = decodeUtf8 $ B16.encode $ P.exportEd25519Signature scSign
  comd <- liftIO $ P.mkCommand' [((pub1, pri1), "notsurewhattopass")] (BC.pack (cmdPayloadTemplate ++ unpack pub1' ++ "\"}]}" ))
  -- let code' = "(module add-module (defun add-one (x) (+ x 1)))"
  -- let payload' = P.execPayload $ P.mkExec code [] defMeta
  -- let cmd = P.mkCommand yourPublicKey yourPrivateKey payload
  -- let c' = decodeUtf8 <$> cmd
  cmdTypeCommand <- liftIO $ P.mkExec "(+ 1 2)" Null def [(P.DynEd25519KeyPair (a,b),[])] [] Nothing (Just "test1")
  let cc = (decodeUtf8 . BSL.toStrict) $ encode cmdTypeCommand
  let c = decodeUtf8 <$> comd
      s3 = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (unpack userK))
      shardId3 = fromInteger $ s3 `rem` 255
      shardIndex3 = Data.List.findIndex (\(x,y) -> shardId3 >= x && shardId3 <= y) (shardRange config)
      cmdText = (decodeUtf8 . BSL.toStrict) $ encode c
  return $ UResponse pub1' pri1' scSignText t1 shardId1 shardIndex1 t2 shardId2 shardIndex2 shardId3 shardIndex3 cmdText ts cc

simpleServerCmd :: IO (P.Command Text)
simpleServerCmd = do
  simpleKeys <- P.DynEd25519KeyPair <$> P.genKeyPair
  P.mkExec  "(+ 1 2)" Null def [(simpleKeys,[])] [] Nothing (Just "test1")


getSign :: Ed25519.PublicKey -> P.Ed25519PrivateKey -> P.PactHash -> Ed25519.Signature
getSign pub pri msg = P.signEd25519 pub pri (P.toUntypedHash msg)

createUser :: Config -> [Connection] -> UserRequest -> Handler Text
createUser config conns req@UserRequest {..} = do
   -- verify the signed user key
   let hashedMsg = P.hash $ BC.pack $ "(create-user " <> signedUserPublicKey <> " " <> (show initialAmount) <> ")"
       verifyResp = P.verifyUserSig hashedMsg (P.ED25519Sig hash) (P.Signer Nothing (pack signedUserPublicKey) Nothing [])
   case verifyResp of
     Right _ -> do
        let s = integerDigest $ sha256 (BSL.fromStrict $ BC.pack signedUserPublicKey)
            shardId = fromInteger $ s `rem` 255
        let shardIndex = Data.List.findIndex (\(x,y) -> shardId >= x && shardId <= y) (shardRange config)
        case shardIndex of
            Just idx -> do
              liftIO $ print idx
              let connection = conns !! idx
              resp :: Either ConnectionError String <- liftIO $ call connection req
              case resp of
                Right "SUCCESS" -> return (pack "User is Created")
                Right err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
            Nothing -> throwError $ err500 { errBody = "INTERNAL_SERVER_ERROR" }
     Left err -> throwError $ err400 { errBody = "Signature Verification Failed"}

transfer :: Config -> [Connection] -> TransferRequest -> Handler Text
transfer config conns req@TransferRequest{..} = do
  let hashedMsg = P.hash $ BC.pack $ "(transfer " <> userKey <> " " <> to <> " " <> (show amount) <> " " <> timestamp <> ")"
      verifyResp = P.verifyUserSig hashedMsg (P.ED25519Sig hash) (P.Signer Nothing (pack userKey) Nothing [])
  case verifyResp of
    Right _ -> do
      let s = integerDigest $ sha256 (BSL.fromStrict $ BC.pack userKey)
          shardId = fromInteger $ s `rem` 255
          shardIndex = Data.List.findIndex (\(x,y) -> shardId >= x && shardId <= y) (shardRange config)
      case shardIndex of
          Just idx -> do
            let connection = conns !! idx
            resp  :: Either ConnectionError String <- liftIO $ call connection req
            case resp of
              Right "SUCCESS" -> return (pack "Transfer is Successful")
              Right err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
              Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
          Nothing -> throwError $ err500 { errBody = "INTERNAL_SERVER_ERROR" }
    Left err -> throwError $ err400 { errBody = "Signature Verification Failed"}

handleSC :: Config -> [Connection] -> PactRequest -> Handler P.RequestKey
handleSC config conns req = do
   case eitherDecodeStrict' (encodeUtf8 $ cmd req) :: Either String (P.Command Text) of
      Right payload -> do
        case P.verifyCommand (encodeUtf8 <$> payload) :: P.ProcessedCommand P.PrivateMeta P.ParsedCode of
            P.ProcSucc r -> do
                let s = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (user req))
                    shardId = fromInteger $ s `rem` 255
                    shardIndex = Data.List.findIndex (\(x,y) -> shardId >= x && shardId <= y) (shardRange config)
                case shardIndex of
                    Just idx -> do
                      let connection = conns !! idx
                      let txt =  pack "" --(decodeUtf8 . BSL.toStrict) $ encode r
                          reqKey = P.cmdToRequestKey payload
                          reqKeys = P.RequestKeys $ pure $ reqKey
                          scReq = SCRequest txt (user req) ((decodeUtf8 . BSL.toStrict) $ encode reqKey) (cmd req) "LOCAL"
                      resp :: Either ConnectionError String
                        <- liftIO $ call connection scReq
                      case resp of
                        Right "SUCCESS" -> return reqKey
                        Right err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                        Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                    Nothing -> throwError $ err400 { errBody = "Unable to find the user" }
            P.ProcFail err -> throwError $ err400 { errBody = BSL.fromStrict $ BC.pack $ show err }
      Left err -> throwError $ err400 { errBody = BSL.fromStrict $ BC.pack $ show err }

deploySC :: Config -> [Connection] -> PactRequest -> Handler P.RequestKey
deploySC config conns req =
  case eitherDecodeStrict' (encodeUtf8 $ cmd req) :: Either String (P.Command Text) of
    Right payload -> do
      case P.verifyCommand (encodeUtf8 <$> payload) :: P.ProcessedCommand P.PrivateMeta P.ParsedCode of
          P.ProcSucc r -> do
              let s = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (user req))
                  shardId = fromInteger $ s `rem` 255
                  shardIndex = Data.List.findIndex (\(x,y) -> shardId >= x && shardId <= y) (shardRange config)
              case shardIndex of
                  Just idx -> do
                    let connection = conns !! idx
                    let txt = pack "" --(decodeUtf8 . BSL.toStrict) $ encode r
                        reqKey = P.cmdToRequestKey payload
                        reqKeys = P.RequestKeys $ pure $ reqKey
                        scReq = SCRequest txt (user req) ((decodeUtf8 . BSL.toStrict) $ encode reqKey) (cmd req) "SEND"
                    resp :: Either ConnectionError String
                      <- liftIO $ call connection scReq
                    case resp of
                      Right "SUCCESS" -> return reqKey
                      Right err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                      Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                  Nothing -> throwError $ err400 { errBody = "Unable to find the user" }
          P.ProcFail err -> throwError $ err400 { errBody = BSL.fromStrict $ BC.pack $ show err }
    Left err -> throwError $ err400 { errBody = BSL.fromStrict $ BC.pack $ show err }

submitTx :: TxRequest -> Handler TxResponse
submitTx req = do
   checkTxStructure req
   mapM checkInputStructure (inputs req)
   mapM checkOutputValue (outputs req)
   checkInputOutputSet req
   sendTransaction req
   return $ TxResponse "Done"

sendTransaction :: TxRequest -> Handler ()
sendTransaction req = do
   -- compact tx
   -- sign compact tx
   -- send compact tx
   undefined

-- Validations
checkTxStructure :: TxRequest -> Handler ()
checkTxStructure req = do
   when (Prelude.null (inputs req)) $
     throwError $ err400  { errBody = "no inputs in tx"}
   when (Prelude.null (outputs req)) $
     throwError $ err400  { errBody = "no outputs in tx"}
   when (Prelude.length (witness req) /=  (Prelude.length $ inputs req)) $
     throwError $ err400  { errBody = "missing witness in tx"}
   -- check for no repeat of inputs.

checkInputStructure :: Input -> Handler ()
checkInputStructure inp = checkOutputValue (prevoutData inp)

checkOutputValue :: Output -> Handler ()
checkOutputValue out =
   when (value out < 1) $
     throwError $ err400 { errBody = "output has zero value" }

checkInputOutputSet :: TxRequest -> Handler ()
checkInputOutputSet req = do
   inputValue <- foldM go 0 (value . prevoutData <$> inputs req)
   outputValue <- foldM go 0 (value <$> outputs req)
   when (inputValue /= outputValue) $
       throwError $ err400 { errBody = "assymetric values"}
   where
    go ac x = if ac + x <= ac
      then throwError $ err400 { errBody = "value overflow"}
      else return $ ac + x

checkWitness :: String -> TxRequest -> Handler ()
checkWitness witness req = do
   when (Prelude.null witness) $ throwError $ err400 {errBody = "missing witness program type"}
   case witness of
     ('0':xs) -> checkp2pkWitness witness req
     _ -> throwError $ err400 { errBody = "unknown witness program type" }

checkp2pkWitness :: String -> TxRequest -> Handler ()
checkp2pkWitness witness req = do
   -- length validation
   -- hash validation
   -- sign validation
   undefined

-- Configs
data Endpoint = Endpoint
  { host :: String
  , port :: Int
  }
  deriving (Generic, FromJSON)

data Config = Config
  { shards :: [Endpoint]
  , shardRange :: [(Int, Int)]
  }
  deriving (Generic, FromJSON)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [filePath, sentinelId] -> do
      eitherConfig <- eitherDecode <$> BSL.readFile filePath
      case eitherConfig of
        Left err -> print err
        Right config -> do
          connections <- mapM (\ep -> connect [] (getHostAddr ep) (getPort ep)) (shards config)
          putStrLn "listening to port 8081..."
          run 8081 (sentinelApp config connections)
    _ -> error "not enought inputs. sentinel config-file sentinelId"
    where
      getHostAddr ep =
       case BS.split (toEnum $ fromEnum '.') (BC.pack $ host ep) of
          [f,s,t,fr] -> (read (BC.unpack f), read (BC.unpack s), read (BC.unpack t), read (BC.unpack fr))
          _ -> error "unexpected host"
      getPort ep = toEnum $ port ep

cmdPayloadTemplate :: String
cmdPayloadTemplate =
    "{\"payload\":{\"cont\":{\"pactId\":\"node0\",\"step\":1,\"rollback\":true,\"data\": {},\"proof\":\"proof\"}},\"nonce\":\"nonce\",\"meta\":{}, \"signers\":[{\"pubKey\":\""

getCurrentTimestamp :: IO String
getCurrentTimestamp = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" currentTime
  return formattedTime

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ethProvider :: Provider
-- ethProvider = HttpProvider "http://localhost:8545"

getEthBalance :: Web3.Address -> IO (Either Web3.Web3Error Quantity)
getEthBalance address = Web3.runWeb3 $ ETH.getBalance address Latest

sendEthTransaction :: Web3.Address -> Web3.Address -> Quantity -> IO (Either Web3.Web3Error BA.HexString)
sendEthTransaction from to amount = Web3.runWeb3 $ do
  let tx =
        Call
          { callFrom = Just from
          , callTo = Just to
          , callGas = Nothing
          , callGasPrice = Nothing
          , callValue = Just amount
          , callData = Nothing
          , callNonce = Nothing
          }
  ETH.sendTransaction tx

data LinkEthRequest = LinkEthRequest
  { ethAddress :: Web3.Address
  , nuchainAddress :: String
  , ethBalance :: Maybe Quantity
  }
  deriving (Generic, FromJSON, ToJSON)

data EthTransferRequest = EthTransferRequest
  { txnHash :: Maybe BA.HexString
  , fromEth :: Web3.Address
  , toEth :: Web3.Address
  , amountInWei :: Integer -- Amount in Wei (1 ETH = 10^18 Wei)
  }
  deriving (Generic, FromJSON, ToJSON)

getEthBalanceHandler :: Config -> [Connection] -> LinkEthRequest -> Handler Text
getEthBalanceHandler config conns req = do
  result <- liftIO $ getEthBalance (ethAddress req)
  case result of
    Left err -> throwError $ err500 { errBody = "Failed to fetch balance from Ethereum." }
    Right wei -> do
      let s = integerDigest $ sha256 (BSL.fromStrict $ BC.pack (nuchainAddress req))
          shardId = fromInteger $ s `rem` 255
      liftIO $ print shardId
      let shardIndex = Data.List.findIndex (\(x,y) -> shardId >= x && shardId <= y) (shardRange config)
      case shardIndex of
        Just idx -> do
          liftIO $ print idx
          let connection = conns !! idx
              txt = (decodeUtf8 . BSL.toStrict) $ encode req
          resp :: Either ConnectionError String <- liftIO $ call connection txt
          case resp of
            Right "SUCCESS" -> return (pack "Wallet Address is Linked Successfully")
            Right err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
            Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
        Nothing -> throwError $ err500 { errBody = "INTERNAL_SERVER_ERROR" }

-- sendEthTransactionHandler :: Config -> [Connection] -> EthTransferRequest -> Handler Text
-- sendEthTransactionHandler conf conns EthTransferRequest{..} = do
--   result <- liftIO $ sendEthTransaction fromEth toEth (fromInteger amountInWei)
--   case result of
--     Left err -> throwError $ err500 { errBody = "Transaction failed." }
--     Right txHash -> do
--       idx <- chooseShard (Prelude.length (shardRange conf))
--       let connection = conns !! idx
--           pendingReq = EthTransferRequest (Just txHash) fromEth toEth amountInWei "PENDING"
--       resp :: Either ConnectionError String <- liftIO $ call connection pendingReq
--       case resp of
--           Right "SUCCESS" -> do
--             liftIO $ forkIO $ monitorTransactionStatus txHash fromEth connection
--             return (pack $ "Transaction submitted successfully. TxHash: " ++ show txHash)
--           Right err -> throwError $ err500 { errBody = BSL.pack $ show err }
--           Left err -> throwError $ err500 { errBody = BSL.pack $ show err }

-- data TxReceipt = TxReceipt
--     { receiptTransactionHash   :: !HexString
--     -- ^ DATA, 32 Bytes - hash of the transaction.
--     , receiptTransactionIndex  :: !Quantity
--     -- ^ QUANTITY - index of the transaction.
--     , receiptBlockHash         :: !(Maybe HexString)
--     -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
--     , receiptBlockNumber       :: !(Maybe Quantity)
--     -- ^ QUANTITY - block number where this transaction was in. null when its pending.
--     , receiptCumulativeGasUsed :: !Quantity
--     -- ^ QUANTITY - The total amount of gas used when this transaction was executed in the block.
--     , receiptGasUsed           :: !Quantity
--     -- ^ QUANTITY - The amount of gas used by this specific transaction alone.
--     , receiptContractAddress   :: !(Maybe Address)
--     -- ^ DATA, 20 Bytes - The contract address created, if the transaction was a contract creation, otherwise null.
--     , receiptLogs              :: ![Change]
--     -- ^ Array - Array of log objects, which this transaction generated.
--     , receiptLogsBloom         :: !HexString
--     -- ^ DATA, 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
--     , receiptStatus            :: !(Maybe Quantity)
--     -- ^ QUANTITY either 1 (success) or 0 (failure)
--     }
--     deriving (Show, Generic)


-- monitorTransactionStatus :: Text -> Text -> Connection -> IO ()
-- monitorTransactionStatus txHash fromEth connection = do
--     let delay = 10 * 1000000
--     let maxRetries = 100
--     result <- ETH.getTransactionReceipt txHash
--     case result of
--         Just (receipt :: TxReceipt) -> do
--           case (receiptStatus receipt) of
--             Nothing | Just ->
            
--           let minedReq = EthTransferRequest (Just txHash) "SUCCESS" (fromEthAddress receipt) (toEthAddress receipt) (gasUsed receipt)
--           _ <- call connection minedReq
--           putStrLn $ "Transaction " ++ show txHash ++ " is mined and status updated."
--         Nothing -> do
--           if maxRetries > 0
--               then do
--                   putStrLn $ "Transaction " ++ show txHash ++ " is still pending. Retrying..."
--                   threadDelay delay
--                   monitorTransactionStatus txHash fromEth connection delay (maxRetries - 1)
--               else
--                   putStrLn $ "Transaction " ++ show txHash ++ " is still pending after max retries."

-- chooseShard :: Int -> IO Int
-- chooseShard n = randomRIO (1, n)







-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Text (Text, pack)
-- import Data.ByteString.Lazy.Char8 as BSL (pack)
-- import qualified Data.ByteString.Char8 as BC
-- import Network.JsonRpc.TinyClient (JsonRpc, JsonRpcError)
-- import Network.Wai.Handler.Warp (Connection)
-- import Servant (Handler, throwError, err500)
-- import Network.Web3 (sendEthTransaction, getTransactionReceipt)

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- module Main where

-- import Codec.Winery
-- import Data.Aeson
-- import Codec.Winery.Class
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BSL
-- import Data.Time.Clock
-- import Data.Time.Format
-- import Data.Scientific
-- import Data.Text
-- import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import qualified Data.List
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Crypto.PubKey.Ed25519 as Ed25519

-- import GHC.Generics

-- import Network.Wai.Handler.Warp
-- import Nuchain.Util.SeedPhrase
-- import Servant
-- import Servant.API
-- import Servant.Server
-- import System.Environment

-- import Data.Digest.Pure.SHA

-- import qualified Pact.Server.API as P
-- import qualified Pact.Types.Exp as P
-- import qualified Pact.Types.Runtime as P
-- import qualified Pact.Types.Command as P
-- import qualified Pact.Types.API as P
-- import qualified Pact.Types.Crypto as P
-- import Pact.JSON.Encode (Encode, toJsonViaEncode)

-- -- RPC imports
-- import Network.RPC.Curryer.Client
-- import Network.RPC.Curryer.Server (localHostAddr, ConnectionError)

-- import Masala.VM
-- import Masala.VM.Types
-- import Masala.Ext.Simple
-- import Data.IORef

-- --- Overlappable instances
-- --
-- instance Encode a => ToJSON (P.Command a) where
--   toJSON = toJsonViaEncode

-- instance ToJSON P.RequestKeys where
--   toJSON = toJsonViaEncode

-- -- Data type for the request to deploy a contract
-- data DeployRequest = DeployRequest
--   { bytecode :: Text
--   , from :: String
--   , gas :: Int
--   } deriving (Generic, Show, FromJSON)

-- -- Data type for the response after deploying a contract
-- data DeployResponse = DeployResponse
--   { contractAddress :: String
--   , status :: String
--   } deriving (Generic, Show, ToJSON)

-- -- Data type for calling a function on a deployed contract
-- data CallRequest = CallRequest
--   { from :: String
--   , to :: String
--   , data :: Text
--   , gas :: Int
--   } deriving (Generic, Show, FromJSON)

-- -- Data type for the response after calling a function on a contract
-- data CallResponse = CallResponse
--   { output :: Text
--   , status :: String
--   } deriving (Generic, Show, ToJSON)

-- type APIs =
--   "submit" :> "tx" :> ReqBody '[JSON] TxRequest :> Post '[JSON] TxResponse
--   :<|> "deploy" :> "contract" :> ReqBody '[JSON] DeployRequest :> Post '[JSON] DeployResponse
--   :<|> "call" :> "contract" :> ReqBody '[JSON] CallRequest :> Post '[JSON] CallResponse
--   :<|> "local" :> ReqBody '[JSON] PactRequest :> Post '[JSON] P.RequestKeys
--   :<|> "create" :> "user" :> ReqBody '[JSON] UserRequest :> Get '[PlainText] Text
--   :<|> "transfer" :> ReqBody '[JSON] TransferRequest :> Get '[JSON] Text
--   :<|> "create" :> "user" :> "details" :> Get '[JSON] CreateUserDetails
--   :<|> "import" :> "user" :> "details" :> ReqBody '[JSON] ImportUserRequest :> Post '[JSON] CreateUserDetails
--   :<|> "transfer" :> "details" :> ReqBody '[JSON] TransferDetailsRequest :> Post '[JSON] TransferDetailsResponse
--   :<|> "getHash" :> ReqBody '[JSON] URequest :> Post '[JSON] UResponse

-- sentinelAPI :: Proxy APIs
-- sentinelAPI = Proxy

-- sentinelServer :: Config -> [Connection] -> Server APIs
-- sentinelServer config conns =
--   submitTx :<|> deployContract config conns :<|> callContract config conns
--   :<|> (handleSC config conns) :<|> (createUser config conns)
--   :<|> (transfer config conns) :<|> createUserDetails
--   :<|> importUserDetails :<|> transferDetails
--   :<|> getHash config conns

-- sentinelApp :: Config -> [Connection] -> Application
-- sentinelApp config conns = serve sentinelAPI (sentinelServer config conns)

-- -- Function to deploy a Solidity contract
-- deployContract :: Config -> [Connection] -> DeployRequest -> Handler DeployResponse
-- deployContract _ _ DeployRequest{..} = do
--     let bytecode' = BC.pack (unpack bytecode)
--         sender = BC.pack from
--         gasLimit = fromIntegral gas

--     -- Run the EVM with the given bytecode
--     result <- liftIO $ runEVMWithMemory bytecode' sender gasLimit
--     case result of
--         Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack err }
--         Right contractAddr -> return $ DeployResponse
--             { contractAddress = contractAddr
--             , status = "Contract deployed successfully"
--             }

-- -- Function to handle a function call on a deployed contract
-- callContract :: Config -> [Connection] -> CallRequest -> Handler CallResponse
-- callContract _ _ CallRequest{..} = do
--     let inputData = BC.pack (unpack data)
--         sender = BC.pack from
--         contractAddress = BC.pack to
--         gasLimit = fromIntegral gas

--     -- Execute the function call using the EVM
--     result <- liftIO $ runEVMWithMemoryForCall inputData contractAddress sender gasLimit
--     case result of
--         Left err -> throwError $ err500 { errBody = BSL.fromStrict $ BC.pack err }
--         Right outputData -> return $ CallResponse
--             { output = decodeUtf8 outputData
--             , status = "Call executed successfully"
--             }

-- -- Function to run the EVM using in-memory state for contract deployment
-- runEVMWithMemory :: ByteString -> ByteString -> Int -> IO (Either String String)
-- runEVMWithMemory bytecode sender gasLimit = do
--     -- Initialize in-memory EVM state.
--     extData <- initializeInMemoryState
--     let prog = toProg (either error id (parse bytecode))
--     let env = emptyVMEnv {
--         _prog = prog,
--         _address = decodeUtf8 sender,
--         _caller = decodeUtf8 sender,
--         _gasLimit = gasLimit
--     }
--     -- Launch the EVM and execute the contract.
--     (result, _) <- launchVM emptyVMState env Nothing
--     case result of
--         Left err -> return (Left err)
--         Right (Final output) -> return (Right (encodeUtf8 output))
--         _ -> return (Left "Unexpected result from EVM")

-- -- Function to run the EVM using in-memory state for contract calls
-- runEVMWithMemoryForCall :: ByteString -> ByteString -> ByteString -> Int -> IO (Either String ByteString)
-- runEVMWithMemoryForCall inputData contractAddr sender gasLimit = do
--     -- Initialize in-memory EVM state for the call.
--     extData <- initializeInMemoryState
--     let env = emptyVMEnv {
--         _callData = inputData,
--         _address = decodeUtf8 contractAddr,
--         _caller = decodeUtf8 sender,
--         _gasLimit = gasLimit
--     }
--     -- Launch the EVM and execute the function call.
--     (result, _) <- launchVM emptyVMState env Nothing
--     case result of
--         Left err -> return (Left err)
--         Right (Final output) -> return (Right (encodeUtf8 output))
--         _ -> return (Left "Unexpected result from EVM")

-- -- Rest of the functions remain the same as previously implemented...

-- main :: IO ()
-- main = do
--   xs <- getArgs
--   case xs of
--     [filePath, sentinelId] -> do
--       eitherConfig <- eitherDecode <$> BSL.readFile filePath
--       case eitherConfig of
--         Left err -> print err
--         Right config -> do
--           connections <- mapM (\ep -> connect [] (getHostAddr ep) (getPort ep)) (shards config)
--           putStrLn "listening to port 8081..."
--           run 8081 (sentinelApp {-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Sentinel where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import GHC.Generics (Generic)
import Servant
import Servant.Server
import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Process (readProcess)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis

-- Data type for contract deployment request
data ContractDeploymentRequest = ContractDeploymentRequest
  { solidityCode :: T.Text
  , walletAddr :: T.Text
  , nonce :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Data type for contract deployment response
data ContractDeploymentResponse = ContractDeploymentResponse
  { contractAddress :: T.Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Data type for calling a contract
data CallContractRequest = CallContractRequest
  { address :: T.Text
  , functionName :: T.Text
  , args :: [T.Text]
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Data type for the call contract response
data CallContractResponse = CallContractResponse
  { result :: T.Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- API definition
type SentinelAPI =
       "deploy" :> ReqBody '[JSON] ContractDeploymentRequest :> Post '[JSON] ContractDeploymentResponse
  :<|> "call"   :> ReqBody '[JSON] CallContractRequest :> Post '[JSON] CallContractResponse

-- Compile Solidity to bytecode and deploy it
deployContract :: Redis.Connection -> ContractDeploymentRequest -> Handler ContractDeploymentResponse
deployContract redisConn req = do
    -- Compile Solidity code
    let solidityFilePath = "/tmp/contract.sol"
    liftIO $ writeFile solidityFilePath (T.unpack $ solidityCode req)
    bytecode <- liftIO $ readProcess "solc" ["--bin", solidityFilePath] ""
    abi <- liftIO $ readProcess "solc" ["--abi", solidityFilePath] ""

    -- Store ABI and Bytecode in Redis
    let contractAddress = "0x" ++ take 40 (hashWithNonce (nonce req))
    liftIO $ Redis.runRedis redisConn $ do
      Redis.set (BS.pack $ "abi:" ++ contractAddress) (BS.pack abi)
      Redis.set (BS.pack $ "bytecode:" ++ contractAddress) (BS.pack bytecode)

    -- Return the contract address
    return $ ContractDeploymentResponse (T.pack contractAddress)

-- Hash nonce to generate a mock contract address
hashWithNonce :: Integer -> String
hashWithNonce nonce = take 64 $ show $ abs (fromIntegral $ nonce * 2654435761)

-- Call a contract function
callContract :: Redis.Connection -> CallContractRequest -> Handler CallContractResponse
callContract redisConn req = do
    -- Fetch ABI and Bytecode from Redis
    let abiKey = "abi:" ++ T.unpack (address req)
    let bytecodeKey = "bytecode:" ++ T.unpack (address req)
    redisResult <- liftIO $ Redis.runRedis redisConn $ do
      abi <- Redis.get (BS.pack abiKey)
      bytecode <- Redis.get (BS.pack bytecodeKey)
      return (abi, bytecode)
    case redisResult of
      (Right (Just abi, Just bytecode)) -> do
          let callData = encodeCallData (functionName req) (args req)
          evmResult <- liftIO $ executeEVM bytecode callData
          return $ CallContractResponse (T.pack evmResult)
      _ -> throwError err404 { errBody = "Contract not found in Redis" }

-- Encode function call data
encodeCallData :: T.Text -> [T.Text] -> String
encodeCallData functionName args =
    "0x" ++ take 8 (sha3 (T.unpack functionName ++ "(" ++ argTypes args ++ ")")) ++ concatMap padLeft args
  where
    argTypes args = T.unpack $ T.intercalate "," (map detectType args)
    padLeft arg = replicate (64 - length arg) '0' ++ T.unpack arg
    sha3 = ... -- SHA3 hashing logic

-- Execute EVM bytecode with provided call data
executeEVM :: String -> String -> IO String
executeEVM bytecode callData = do
    let evmCommand = "/path/to/evm-interpreter"
    let args = ["run", "--code", bytecode, "--data", callData]
    result <- readProcess evmCommand args ""
    return result

-- Server implementation
server :: Redis.Connection -> Server SentinelAPI
server redisConn = deployContract redisConn :<|> callContract redisConn

-- Main function to run the server
main :: IO ()
main = do
    redisConn <- Redis.checkedConnect Redis.defaultConnectInfo
    run 8080 (serve (Proxy :: Proxy SentinelAPI) (server redisConn))
config connections)
--     _ -> error "not enough inputs. sentinel config-file sentinelId"
--     where
--       getHostAddr ep =
--        case BS.split (toEnum $ fromEnum '.') (BC.pack $ host ep) of
--           [f,s,t,fr] -> (read (BC.unpack f), read (BC.unpack s), read (BC.unpack t), read (BC.unpack fr))
--           _ -> error "unexpected host"
--       getPort ep = toEnum $ port ep


