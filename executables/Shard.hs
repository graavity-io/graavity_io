{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Winery
import Control.Exception (throwIO)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.Scientific
import qualified Data.Text as DT
import Data.Maybe
import Data.Text.Encoding

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.Redis as Redis
import GHC.Generics
import Pact.Types.Command as P

import Network.RPC.Curryer.Client
import Network.RPC.Curryer.Server
import System.Environment
import System.Random
import Text.Read (readMaybe)
import Data.Time.Clock
import Data.Time.Format

import Nuchain.Consensus.Raft.Client (clientRepl)
import Servant hiding (serve)
import Configuration.Dotenv (loadFile, defaultConfig)

import Network.Ethereum.Api.Types (Quantity)
import qualified Network.Web3 as Web3

data RPCRequest
  = SC SCRequest (MVar String)
  | UC UserRequest (MVar String)
  | TC TransferRequest (MVar String)
  | LINK DT.Text (MVar String)
  deriving (Generic)

data SCRequest =
  SCRequest
    { code :: DT.Text -- P.Command (P.Payload P.PrivateMeta P.ParsedCode)
    , publicKey :: String
    , key :: DT.Text -- P.RequestKeys
    , originalReq :: DT.Text -- P.Command DT.Text
    , localOrSend :: String -- "LOCAL" | "SEND"
    }
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SCRequest

data PactType =
  PactType
    { cmdReq :: DT.Text -- original req
    , lOrS :: String -- "LOCAL" | "SEND"
    }
  deriving (Generic, Show, ToJSON)
  deriving Serialise via WineryVariant PactType

-- add the call to RPC
data UserRequest =
  UserRequest
    { signedUserPublicKey :: String
    , initialAmount :: Scientific
    , hash :: DT.Text
    }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
  deriving Serialise via WineryVariant UserRequest

-- instance RaftStateMachinePure DT.Text UserRequest where
--   data RaftStateMachinePureError DT.Text UserRequest = SomeError [Char]
--    deriving (Show, Generic, S.Serialize)

data TransferRequest =
  TransferRequest
    { userKey :: String
    , to :: String
    , amount :: Scientific
    , hashTR :: DT.Text
    }
  deriving (Generic, FromJSON, Show)
  deriving Serialise via WineryVariant TransferRequest

data ConsensusParams = ConsensusParams
  { consensusType :: ConsensusType
  , weightage     :: Double
  } deriving (Generic, FromJSON, Show)

data ConsensusType
  = ProofOfStake
  | ProofOfCapacity
  | Raft
  deriving (Generic, FromJSON, Show)

data Config =
  Config
    { pact :: Endpoint
    , atomizer :: Endpoint
    , gas :: Scientific
    , consensusParams :: ConsensusParams
    , adminAddress :: String
    }
  deriving (Generic, FromJSON, Show)

-- copied type from executables/Sentinels.hs
data Endpoint =
  Endpoint
    { host :: String
    , port :: Int
    }
  deriving (Generic, FromJSON, Show)

-- copied from executable/Atomizer.hs
data Log
  = GasFee GasFeeLog
  | Pact [PactLog]
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant Log

data GasFeeLog =
  GasFeeLog
    { pactRequest :: Maybe DT.Text --Maybe (P.Command DT.Text)
    , userKey :: String
    , requestKeys :: Maybe DT.Text --Maybe (P.RequestKey)
    , gasFee :: Scientific
    , txnDetails :: TxnDetails
    , txnHash :: Maybe DT.Text
    , blockNum :: Maybe Int
    , validatorName :: Maybe String
    , votePercent :: Maybe Float
    }
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant GasFeeLog

data TxnDetails =
  TxnDetails
    { txnId :: Maybe Int
    , fromKey :: String
    , fromKeyAmt :: Maybe Scientific
    , toKey :: Maybe String
    , toKeyAmt :: Maybe Scientific
    , txnAmt :: Maybe Scientific
    , txnTimestamp :: String -- todo: confirm the type
    , txnStatus :: TxnStatus
    }
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant TxnDetails

data TxnStatus =
  SUCCESS | FAILED | PENDING
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant TxnStatus

newtype PactLog =
  PactLog
    { result :: DT.Text -- P.CommandResult Hash
    }
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant PactLog

data LinkEthRequest = LinkEthRequest
  { ethAddress :: Web3.Address
  , nuchainAddress :: String
  , ethBalance :: Maybe Quantity
  }
  deriving (Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [filePath, port] -> do
      eitherConfig :: Either String Config <- eitherDecode <$> BSL.readFile filePath
      case eitherConfig of
        Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
        Right cfg -> do
          connection <- connect [] (getHostAddr $ pact cfg) (getPort $ pact cfg)
          atconnection <- connect [] (getHostAddr $ atomizer cfg) (getPort $ atomizer cfg)
          -- _ <- loadFile defaultConfig
          dbConn <- initPostgres
          redisConn <- initRedis
          var <- newChan :: IO (Chan RPCRequest)
          void $ forkIO $ forever $ do
            req <- readChan var
            case req of
              SC x resVar -> handleSCRequest x resVar connection atconnection dbConn redisConn cfg
              UC x resVar -> handleUCRequest x resVar atconnection dbConn redisConn cfg
              TC x resVar -> handleTCRequest x resVar atconnection dbConn redisConn cfg
              LINK x resVar -> handleLinkRequest x resVar atconnection dbConn redisConn cfg
          putStrLn $ "starting shard RPC server..." ++ port
          void $ serve userRequestHandlers var allHostAddrs (toEnum (read port :: Int)) Nothing
    _ -> error "not enough input. shard config file"
  where
    getHostAddr ep = case BS.split (toEnum $ fromEnum '.') (BC.pack $ host ep) of
      [f, s, t, fr] -> (read (BC.unpack f), read (BC.unpack s), read (BC.unpack t), read (BC.unpack fr))
      _ -> error "unexpected host"
    getPort ep = toEnum $ port ep

initPostgres :: IO Postgres.Connection
initPostgres = do
  dbHost <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
  dbName <- fromMaybe "sharddb" <$> lookupEnv "POSTGRES_DB"
  dbUser <- fromMaybe "user" <$> lookupEnv "POSTGRES_USER"
  dbPassword <- fromMaybe "password" <$> lookupEnv "POSTGRES_PASSWORD"
  Postgres.connect Postgres.defaultConnectInfo {
    Postgres.connectHost = dbHost,
    Postgres.connectDatabase = dbName,
    Postgres.connectUser = dbUser,
    Postgres.connectPassword = dbPassword
  }

initRedis :: IO Redis.Connection
initRedis = do
  redisHost <- fromMaybe "localhost" <$> lookupEnv "REDIS_HOST"
  redisPort <- fromMaybe "6379" <$> lookupEnv "REDIS_PORT"
  redisPassword <- lookupEnv "REDIS_PASSWORD"
  let connectInfo = Redis.defaultConnectInfo {
        Redis.connectHost = redisHost,
        Redis.connectPort = Redis.PortNumber (read redisPort),
        Redis.connectAuth = fmap BC.pack redisPassword
      }
  Redis.checkedConnect connectInfo

handleSCRequest :: SCRequest -> MVar String -> Connection -> Connection -> Postgres.Connection -> Redis.Connection -> Config -> IO ()
handleSCRequest scr@SCRequest{..} resVar connection atconnection dbConn redisConn cfg = do
  let (constGasFee, consensus, admin) = (gas cfg, consensusParams cfg, adminAddress cfg)
  userBalance <- getUserBalance redisConn dbConn publicKey
  adminBalance <- getUserBalance redisConn dbConn admin
  case (userBalance, adminBalance) of
    (Just cg, Just ab) | cg > constGasFee -> do
      result <- validated (SC scr resVar) consensus
      if result
        then do
          updateUserBalance dbConn redisConn publicKey (cg - constGasFee)
          updateUserBalance dbConn redisConn admin (ab + constGasFee)
        else putMVar resVar "Error in validation"
      resp :: Either ConnectionError String <- call connection ((decodeUtf8 . BSL.toStrict) $ encode (PactType originalReq localOrSend))
      case resp of
        Left err -> putMVar resVar (show err)
        Right _ -> do
          now <- getCurrentTimestamp
          let txnDetail = TxnDetails Nothing publicKey (Just $ cg - constGasFee) Nothing Nothing (Just 0.0) now PENDING
              gfl = GasFee $ GasFeeLog (Just originalReq) publicKey (Just key) constGasFee txnDetail (getSCHash originalReq) Nothing Nothing Nothing
              txt = (decodeUtf8 . BSL.toStrict) $ encode gfl
          atomizerRes :: Either ConnectionError DT.Text <- call atconnection txt
          case atomizerRes of
            Left err -> putMVar resVar (show err)
            Right _ -> putMVar resVar "SUCCESS"
    _ -> putMVar resVar "Insufficient gas fee"

handleUCRequest :: UserRequest -> MVar String -> Connection -> Postgres.Connection -> Redis.Connection -> Config -> IO ()
handleUCRequest userRequest resVar atconnection dbConn redisConn cfg = do
  let (constGasFee, consensus) = (gas cfg, consensusParams cfg)
  user <- getUserBalance redisConn dbConn (signedUserPublicKey userRequest)
  case user of
    Just _ ->
      putMVar resVar "User wallet address already exists in db!"
    Nothing -> do
      result <- validated (UC userRequest resVar) consensus
      if result
        then do
          createUserBalance dbConn redisConn (signedUserPublicKey userRequest) (initialAmount userRequest)
        else putMVar resVar "Error in validation"
      now <- getCurrentTimestamp
      let txnDetail = TxnDetails Nothing (signedUserPublicKey userRequest) (Just $ initialAmount userRequest) Nothing Nothing (Just $ initialAmount userRequest) now SUCCESS
          gfl = GasFee $ GasFeeLog Nothing (signedUserPublicKey userRequest) Nothing constGasFee txnDetail (Just $ hash userRequest) Nothing Nothing Nothing
          txt = (decodeUtf8 . BSL.toStrict) $ encode gfl
      resp :: Either ConnectionError DT.Text <- call atconnection txt
      case resp of
        Left err -> putMVar resVar (show err)
        Right _ -> putMVar resVar "SUCCESS"

handleTCRequest :: TransferRequest -> MVar String -> Connection -> Postgres.Connection -> Redis.Connection -> Config -> IO ()
handleTCRequest tr@TransferRequest{..} resVar atconnection dbConn redisConn cfg = do
  let (constGasFee, consensus, admin) = (gas cfg, consensusParams cfg, adminAddress cfg)
  if userKey == to
    then putMVar resVar "Can't send to the same user!"
    else do
      currentGas <- getUserBalance redisConn dbConn userKey
      case currentGas of
        Nothing -> putMVar resVar "User wallet address not found in db!"
        Just oldG -> do
          let amount' = amount + constGasFee
          if oldG >= amount'
            then do
              toGas <- getUserBalance redisConn dbConn to
              adminBalance <- getUserBalance redisConn dbConn admin
              case (toGas, adminBalance)  of
                (Just newg, Just ab) -> do
                  result <- validated (TC tr resVar) consensus
                  if result
                    then do
                      updateUserBalance dbConn redisConn to (newg + amount)
                      updateUserBalance dbConn redisConn userKey (oldG - amount')
                      updateUserBalance dbConn redisConn admin (ab + constGasFee)
                    else putMVar resVar "Error in validation"
                  now <- getCurrentTimestamp
                  let txnDetail = TxnDetails Nothing userKey (Just $ oldG - amount') (Just to) (Just (newg + amount)) (Just amount) now SUCCESS
                      gf = GasFee $ GasFeeLog Nothing userKey Nothing constGasFee txnDetail (Just hashTR) Nothing Nothing Nothing
                      txt = (decodeUtf8 . BSL.toStrict) $ encode gf
                  resp :: Either ConnectionError DT.Text <- liftIO $ call atconnection txt
                  case resp of
                    Left err -> putMVar resVar (show err)
                    Right _ -> putMVar resVar "SUCCESS"
                _ -> putMVar resVar "To wallet address not found in db!"
            else do
              toGas <- getUserBalance redisConn dbConn to
              case toGas of
                Nothing -> putMVar resVar "To wallet address not found in db!"
                Just newg -> do
                  now <- liftIO getCurrentTimestamp
                  let txnDetail = TxnDetails Nothing userKey (Just oldG) (Just to) (Just newg) (Just amount) now FAILED
                      gf = GasFee $ GasFeeLog Nothing userKey Nothing constGasFee txnDetail (Just hashTR) Nothing Nothing Nothing
                      txt = (decodeUtf8 . BSL.toStrict) $ encode gf
                  resp :: Either ConnectionError DT.Text <- call atconnection txt
                  case resp of
                    Left err -> putMVar resVar (show err)
                    Right _ -> putMVar resVar "Insufficient funds"

handleLinkRequest :: DT.Text -> MVar String -> Connection -> Postgres.Connection -> Redis.Connection -> Config -> IO ()
handleLinkRequest txt resVar atconnection dbConn redisConn cfg = do
  case eitherDecodeStrict' (encodeUtf8 txt) :: Either String LinkEthRequest of
    Left err -> putMVar resVar $ "Error decoding request: " ++ err
    Right LinkEthRequest{..} -> do
      existingLinks <- Postgres.query dbConn
        "SELECT eth_address FROM eth_nuchain_link WHERE nuchain_wallet_id = ?"
        (Postgres.Only nuchainAddress)
      case existingLinks of
        [] -> do
          _ <- Postgres.execute dbConn
            "INSERT INTO eth_nuchain_link (nuchain_wallet_id, eth_address, created_at) VALUES (?, ?, CURRENT_TIMESTAMP)"
            (nuchainAddress, (show ethAddress))
          putMVar resVar "New ETH link successfully added."
        [(existingEthAddress)] | existingEthAddress /= (show ethAddress) -> 
          putMVar resVar "Error: This Nuchain wallet already has an ETH link. Only one ETH account can be linked per Nuchain account."
        [(existingEthAddress)] | existingEthAddress == (show ethAddress) -> 
          putMVar resVar "This ETH account is already linked to the Nuchain wallet."
        _ -> putMVar resVar "Unexpected error."

-- Improved Redis Cache Fetch and Fallback to PostgreSQL
getUserBalance :: Redis.Connection -> Postgres.Connection -> String -> IO (Maybe Scientific)
getUserBalance redisConn dbConn publicKey = do
  cachedBalance <- Redis.runRedis redisConn $ Redis.get (BC.pack $ "wa:" ++ publicKey)
  case cachedBalance of
    Right (Just balanceStr) -> return $ readMaybe $ BC.unpack balanceStr
    _ -> do
      res <- Postgres.query dbConn "SELECT balance FROM users WHERE public_key = ?" (Postgres.Only publicKey)
      case res of
        [Postgres.Only balance] -> do
          _ <- Redis.runRedis redisConn $ Redis.set (BC.pack $ "wa:" ++ publicKey) (BC.pack $ show balance)
          return $ Just balance
        _ -> return Nothing

-- Write Back to PostgreSQL and Update Redis Cache
updateUserBalance :: Postgres.Connection -> Redis.Connection -> String -> Scientific -> IO ()
updateUserBalance dbConn redisConn publicKey newBalance = do
  _ <- Postgres.execute dbConn "UPDATE users SET balance = ? WHERE public_key = ?" (newBalance, publicKey)
  _ <- Redis.runRedis redisConn $ Redis.set (BC.pack $ "wa:" ++ publicKey) (BC.pack $ show newBalance)
  return ()

createUserBalance :: Postgres.Connection -> Redis.Connection -> String -> Scientific -> IO ()
createUserBalance dbConn redisConn publicKey newBalance = do
  _ <- Postgres.execute dbConn "INSERT INTO users (public_key, balance) VALUES (?, ?)" (publicKey, newBalance)
  _ <- Redis.runRedis redisConn $ Redis.set (BC.pack $ "wa:" ++ publicKey) (BC.pack $ show newBalance)
  return ()

userRequestHandlers :: RequestHandlers (Chan RPCRequest)
userRequestHandlers =
  [ RequestHandler $ \state req -> do
      resVar <- newEmptyMVar
      writeChan (connectionServerState state) (SC req resVar)
      result <- takeMVar resVar
      return result
  , RequestHandler $ \state req -> do
      resVar <- newEmptyMVar
      writeChan (connectionServerState state) (UC req resVar)
      result <- takeMVar resVar
      return result
  , RequestHandler $ \state req -> do
      resVar <- newEmptyMVar
      writeChan (connectionServerState state) (TC req resVar)
      result <- takeMVar resVar
      return result
  , RequestHandler $ \state req -> do
      resVar <- newEmptyMVar
      writeChan (connectionServerState state) (LINK req resVar)
      result <- takeMVar resVar
      return result
  ]

getCurrentTimestamp :: IO String
getCurrentTimestamp = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" currentTime
  return formattedTime

getSCHash :: DT.Text -> Maybe DT.Text
getSCHash pr =
  case eitherDecodeStrict' (encodeUtf8 $ pr) :: Either String (P.Command DT.Text) of
    Left _ -> Nothing
    Right pl ->
      if null (P._cmdSigs pl)
        then Nothing
        else
          case head (P._cmdSigs pl) of
            P.ED25519Sig txt -> Just txt
            _ -> Nothing

validateConsensus :: ConsensusParams -> IO Bool
validateConsensus ConsensusParams{..} = 
  case consensusType of
    ProofOfStake     -> validateProofOfStake weightage
    ProofOfCapacity  -> validateProofOfCapacity weightage
    Raft             -> validateRaft

validateProofOfStake :: Double -> IO Bool
validateProofOfStake weightage = do
  -- Simulate stake validation with some randomness influenced by weightage
  stakeValidation <- randomRIO (0, 1 :: Double)
  return (stakeValidation < weightage) -- weightage -> stake factor

validateProofOfCapacity :: Double -> IO Bool
validateProofOfCapacity weightage = do
  -- Simulate capacity-based validation
  capacityValidation <- randomRIO (0, 1 :: Double)
  return (capacityValidation < weightage) -- weightage -> capacity Factor

validateRaft :: IO Bool
validateRaft = do
  -- Raft voting process
  return True

validated :: RPCRequest -> ConsensusParams -> IO Bool
validated request consensus = return True
  -- boolean <- validateConsensus consensus
  -- if not boolean
  --   then return False
  --   else
  --     case request of
  --       SC scr resVar -> undefined
  --       UC ur resVar -> undefined
  --       TC tc resVar -> undefined

validatorClient = do
  clientRepl