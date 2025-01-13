{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson (Value(..), decodeStrict, fromJSON, toJSON, Result(..), FromJSON, ToJSON, encode, eitherDecodeStrict')
import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Scientific
import Data.String
import Data.Time.Format
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BC
import Network.Wai.Handler.Warp
import Servant
import Pact.Types.Command as P
import Configuration.Dotenv (loadFile, defaultConfig)
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import qualified Database.Redis as Redis
import Kafka.Producer
import System.Environment (lookupEnv)

data Log =
  GasFee GasFeeLog
  | Pact [PactLog]
  deriving (Generic, FromJSON)

data GasFeeLog = GasFeeLog
 { pactRequest :: Maybe Text --Maybe (P.Command Text)
 , userKey :: String
 , requestKeys :: Maybe Text --Maybe (P.RequestKeys)
 , gasFee :: Scientific
 , txnDetails :: TxnDetails
 , txnHash :: Maybe Text
 , blockNum :: Maybe Int
 , validatorName :: Maybe String
 , votePercent :: Maybe Float
 }
 deriving (Generic, FromJSON, Show, ToJSON)

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
  deriving (Generic, FromJSON, ToJSON, Show)

data TxnStatus =
  SUCCESS | FAILED | PENDING
  deriving (Generic, Show, FromJSON, ToJSON, Read)

newtype PactLog = PactLog
  { result :: Text -- P.CommandResult Hash
  }
  deriving (Generic, FromJSON)

data Request = Request
  { block_number :: Maybe Int
  , req_transaction_hash :: Maybe String
  , address :: Maybe String
  , request_key :: Maybe String
  }
  deriving (Generic, FromJSON)

instance ToJSON Request

data TransactionInfo = TransactionInfo
  { id :: Maybe Int
  , txn_hash :: Maybe String
  , method :: Maybe String
  , block :: Maybe Int
  , age :: Maybe String
  , from_wallet :: Maybe String
  , to_wallet :: Maybe String
  , value :: Maybe Int
  , transaction_fee :: Maybe Scientific
  , amount :: Maybe Scientific
  , reqKey :: Maybe String
  , status :: Maybe String
  , timestamp :: Maybe String -- todo: confirm the type
  , gas :: Maybe Scientific
  , transaction_action :: Maybe String
  , validator :: Maybe String
  , validationVotePercent :: Maybe Float
  }
  deriving (Generic, Show, FromJSON)

instance ToJSON TransactionInfo

data BlockInfo = BlockInfo
  { blockNumber :: Int
  , previousHash :: Text
  , blockHash :: Text
  , timestamp :: UTCTime
  , nonce :: Int
  , transactions :: [TxInDb]
  , transactionsCount :: Int
  -- todo: check
  , status :: Maybe String
  , fee :: Maybe Scientific
  , size :: Maybe String
  , gas :: Maybe Scientific
  , validator_type :: Maybe String
  , tx_hash :: Maybe Text
  }
  deriving (Generic, Show, FromJSON)

instance ToJSON BlockInfo

data Address = Address
  { addressW :: String
  , balance :: Maybe Scientific
  , value :: Maybe Int
  , last_transaction_sent :: Maybe Text -- hash of first txn
  , first_transaction_sent :: Maybe Text -- hash of last txn
  , transactions :: Maybe [TxInDb]
  , details :: Maybe Details -- for wallet address -> Nothing
  }
  deriving (Generic)

instance ToJSON Address

data Details = Details
  { contract_name :: Maybe String -- | USDTPRO
  , compiler_version :: Maybe String
  , optimization_version :: Maybe String
  , contract_source_code :: String
  , contract_abi :: [Contract]
  }
  deriving (Generic)

instance ToJSON Details

data Contract = Contract
  { name :: Maybe String
  , _type :: String
  , inputs :: [Input]
  , outputs :: Maybe [Input]
  , stateMutability :: Maybe String -- Non payable | Payable | view | pure
  , anonymous :: Maybe Bool
  }
  deriving (Generic)

instance ToJSON Contract

data Input = Input
  { name :: String
  , _type :: String
  , indexed :: Maybe Bool
  , internalType :: String
  }
  deriving (Generic)

instance ToJSON Input

data BlockDetails = BlockDetails
  { total_block_number :: Int
  , total_transactions :: Int
  , total_smart_contracts :: Int
  }
  deriving (Generic)

instance ToJSON BlockDetails

newtype CurrentBlock = CurrentBlock
  { currentBlockNumber :: Int
  }
  deriving (Generic, FromRow)

instance ToJSON CurrentBlock

data TxInDb =
  TxInDb
    { transaction_id :: Maybe Int
    , block_number :: Maybe Int
    , transaction_hash :: Maybe Text
    , request_key :: Maybe Text
    , from_wallet :: String
    , to_wallet :: Maybe String
    , from_wallet_balance :: Maybe Scientific
    , to_wallet_balance :: Maybe Scientific
    , transaction_amount :: Maybe Scientific
    , gas_fee :: Scientific
    , timestamp :: String
    , status :: TxnStatus
    , validator_name :: Maybe String
    , vote_percent :: Maybe Float
    } deriving (Show, Generic, ToJSON, FromJSON)

data TransactionQuery
  = ByBlockNumber Int
  | ByTransactionId Int
  | ByTransactionHash String
  | ByRequestKey String
  deriving (Show)

data Info
  = Block BlockInfo
  | Transaction TxInDb
  deriving (Show, Generic, ToJSON)

type APIs =
  "block" :> "number" :> ReqBody '[JSON] Request :> Post '[JSON] BlockInfo
  :<|> "transaction" :> "hash" :> ReqBody '[JSON] Request :> Post '[JSON] TxInDb
  :<|> "wallet" :> "address" :> ReqBody '[JSON] Request :> Post '[JSON] Address
  :<|> "smart" :> "contract" :> "address" :> ReqBody '[JSON] Request :> Post '[JSON] TxInDb
  :<|> "get" :> "details" :> Get '[JSON] BlockDetails
  :<|> "current" :> "block" :> Get '[JSON] CurrentBlock
  :<|> "check" :> "balance" :> ReqBody '[JSON] Request :> Post '[JSON] Scientific
  :<|> "recent" :> "blocks" :> Get '[JSON] [BlockInfo]
  :<|> "recent" :> "transactions" :> Get '[JSON] [TxInDb]
  :<|> "recent" :> "smart" :> "contracts" :> Get '[JSON] [TxInDb]
  :<|> "blocks" :> QueryParam "timeInSeconds" NominalDiffTime :> Get '[JSON] Int
  :<|> "verify" :> "address" :> ReqBody '[JSON] Request :> Post '[JSON] Bool

watchTowerApi :: Proxy APIs
watchTowerApi = Proxy

watchTowerServer :: Postgres.Connection -> Redis.Connection -> Server APIs
watchTowerServer dbConn redisConn =
  getBlockInfo dbConn redisConn :<|> getTransactionInfo dbConn redisConn
  :<|> getWalletInfo dbConn redisConn :<|> getSmartContractInfo dbConn redisConn
  :<|> getDetails dbConn redisConn :<|> getCurrentBlock dbConn redisConn
  :<|> checkBalance dbConn redisConn :<|> getRecentBlocks dbConn redisConn
  :<|> getRecentTxDetails dbConn redisConn :<|> getRecentScDetails dbConn redisConn
  :<|> getBlocksWithTime dbConn redisConn :<|> verifyAddress dbConn redisConn

watchTowerApp :: Postgres.Connection -> Redis.Connection -> Application
watchTowerApp dbConn redisConn = serve watchTowerApi (watchTowerServer dbConn redisConn)

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

main :: IO ()
main = do
  -- _ <- loadFile defaultConfig
  dbConn <- initPostgres
  redisConn <- initRedis
  putStrLn "listening to port 8080..."
  run 8080 (watchTowerApp dbConn redisConn)

queryRedis :: Redis.Connection -> BC.ByteString -> IO (Maybe BSL.ByteString)
queryRedis redisConn key = do
  result <- Redis.runRedis redisConn $ Redis.get key
  return $ either (const Nothing) (fmap BSL.fromStrict) result

cacheToRedis :: Redis.Connection -> BC.ByteString -> BSL.ByteString -> IO ()
cacheToRedis redisConn key value = do
  _ <- Redis.runRedis redisConn $ Redis.set key (BSL.toStrict value)
  return ()

queryRedisList :: Redis.Connection -> BC.ByteString -> IO (Maybe [BC.ByteString])
queryRedisList redisConn key = do
  result <- Redis.runRedis redisConn $ Redis.lrange key 0 (-1)
  case result of
    Right list -> return $ Just list
    Left _ -> return Nothing

cacheToRedisList :: Redis.Connection -> BC.ByteString -> [BSL.ByteString] -> IO ()
cacheToRedisList redisConn key value = do
  _ <- Redis.runRedis redisConn $ Redis.rpush key (BSL.toStrict <$> value)
  return ()

formatToGMTString :: UTCTime -> String
formatToGMTString = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

fetchFromDBAndCache
  :: TransactionQuery
  -> Redis.Connection
  -> BC.ByteString
  -> Postgres.Connection
  -> IO (Maybe Info)
fetchFromDBAndCache txQuery redisConn key dbConn = do
  dbResult <- fetchFromDB dbConn txQuery
  case dbResult of
    Just (Block blockInfo) -> do
      cacheToRedis redisConn key (encode blockInfo)
      return $ Just (Block blockInfo)
    Just (Transaction txInfo) -> do
      cacheToRedis redisConn key (encode txInfo)
      return $ Just (Transaction txInfo)
    Nothing -> return Nothing

buildBlockInfo :: Int -> Text -> UTCTime -> Int -> Text -> Int-> [TxInDb] -> Info
buildBlockInfo blockNum prevHash ts nonce' bHash txnCount gfl = Block BlockInfo
  { blockNumber = blockNum
  , previousHash = prevHash
  , blockHash = bHash
  , timestamp = ts
  , nonce = nonce'
  , transactions = gfl
  , transactionsCount = txnCount
  , status = Nothing
  , fee = Nothing
  , size = Nothing
  , gas = Nothing
  , validator_type = Nothing
  , tx_hash = Nothing
  }

fetchFromDB :: Postgres.Connection -> TransactionQuery -> IO (Maybe Info)
fetchFromDB dbConn (ByBlockNumber blockNum) = do
  query <- readFile "queries/fetch_block.sql"
  result <- Postgres.query dbConn (fromString query) (Postgres.Only blockNum)
  case result of
    [(blockNum, prevHash, ts, nonce', bHash, txnCount, transactionsJson :: Value)] -> do
      case fromJSON transactionsJson :: Result [TxInDb] of
        Success gfl -> return $ Just (buildBlockInfo blockNum prevHash ts nonce' bHash txnCount gfl)
        Error err -> do
          putStrLn $ "Error decoding transactions to TxInDb: " ++ err
          return $ Just (buildBlockInfo blockNum prevHash ts nonce' bHash txnCount [])
    _ -> return Nothing

fetchFromDB dbConn (ByTransactionHash txnHash)  = do
  let query = "SELECT transaction_id, block_number, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount, gas_fee, status, timestamp FROM transactions WHERE transaction_hash = ?;"
  result <- Postgres.query dbConn query (Postgres.Only txnHash)
  case result of
    [(txnId, blockNum, reqKey, fromWallet, toWallet, fromBalance, toBalance, value, gasFee, status, timestamp)] ->
      return $ Just (Transaction TxInDb
        { transaction_id = Just txnId
        , block_number = Just blockNum
        , transaction_hash = Just (pack txnHash)
        , request_key = reqKey
        , from_wallet = fromWallet
        , to_wallet = toWallet
        , from_wallet_balance = Just fromBalance
        , to_wallet_balance = toBalance
        , transaction_amount = Just value
        , gas_fee = gasFee
        , timestamp = formatToGMTString timestamp
        , status = read status :: TxnStatus
        , validator_name = Nothing
        , vote_percent = Nothing
        })
    _ -> return Nothing

fetchFromDB dbConn (ByRequestKey reqKey)  = do
  let query = "SELECT transaction_id, block_number, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount, gas_fee, status, timestamp FROM transactions WHERE request_key = ?;"
  result <- Postgres.query dbConn query (Postgres.Only reqKey)
  case result of
    [(txnId, blockNum, txnHash, reqKey, fromWallet, toWallet, fromBalance, toBalance, value, gasFee, status, timestamp)] ->
      return $ Just (Transaction TxInDb
        { transaction_id = Just txnId
        , block_number = Just blockNum
        , transaction_hash = Just txnHash
        , request_key = reqKey
        , from_wallet = fromWallet
        , to_wallet = toWallet
        , from_wallet_balance = Just fromBalance
        , to_wallet_balance = toBalance
        , transaction_amount = Just value
        , gas_fee = gasFee
        , timestamp = formatToGMTString timestamp
        , status = read status :: TxnStatus
        , validator_name = Nothing
        , vote_percent = Nothing
        })
    _ -> return Nothing

getBlockInfo :: Postgres.Connection -> Redis.Connection -> Request -> Handler BlockInfo
getBlockInfo dbConn redisConn Request{..} = do
  case block_number of
    Nothing -> throwError $ err404 { errBody = "Block Number input not given" }
    Just blockNum -> do
      let redisKey = BC.pack $ "block:" ++ show blockNum
      redisResult <- liftIO $ queryRedis redisConn redisKey
      case redisResult of
        Just result ->
          case eitherDecodeStrict' (BSL.toStrict result) :: Either String BlockInfo of
            Right blockInfo -> return blockInfo
            Left err -> do
              liftIO $ print err
              throwError err500 { errBody = "Error decoding block info from Redis" }
        Nothing -> do
          dbResult <- liftIO $ fetchFromDBAndCache (ByBlockNumber blockNum) redisConn redisKey dbConn
          case dbResult of
            Just (Block blockInfo) -> return blockInfo
            _ -> throwError err404 { errBody = "Block not found" }

getTransactionInfo :: Postgres.Connection -> Redis.Connection -> Request -> Handler TxInDb
getTransactionInfo dbConn redisConn Request{..} = do
  case req_transaction_hash of
    Nothing -> throwError $ err404 { errBody = "Transaction Hash input not given" }
    Just txnHash -> do
      let redisKey = BC.pack $ "txn:" ++ txnHash
      redisResult <- liftIO $ queryRedis redisConn redisKey
      case redisResult of
        Just result ->
          case eitherDecodeStrict' (BSL.toStrict result) :: Either String TxInDb of
            Right txnInfo -> return txnInfo
            Left err -> do
              liftIO $ print err
              throwError err500 { errBody = "Error decoding transaction info from Redis" }
        Nothing -> do
          dbResult <- liftIO $ fetchFromDBAndCache (ByTransactionHash txnHash) redisConn redisKey dbConn
          case dbResult of
            Just (Transaction txnInfo) -> return txnInfo
            _ -> throwError err404 { errBody = "Transaction not found" }

getSmartContractInfo :: Postgres.Connection -> Redis.Connection -> Request -> Handler TxInDb
getSmartContractInfo dbConn redisConn Request{..} = do
  case request_key of
    Nothing -> throwError $ err404 { errBody = "Smart Contract Address input not given" }
    Just reqKeyValue -> do
      let redisKey = BC.pack $ "smart_contract:" ++ reqKeyValue
      redisResult <- liftIO $ queryRedis redisConn redisKey
      case redisResult of
        Just cachedData ->
          case eitherDecodeStrict' (BSL.toStrict cachedData) of
            Right scTxn -> return scTxn
            Left _ -> throwError err500 { errBody = "Failed to decode cached smart contract transaction." }
        Nothing -> do
          dbResult <- liftIO $ fetchFromDBAndCache (ByRequestKey reqKeyValue) redisConn redisKey dbConn
          case dbResult of
            Just (Transaction txnInfo) -> return txnInfo
            Nothing -> throwError err404 { errBody = "Smart Contract Transaction not found" }

getCurrentBlock :: Postgres.Connection -> Redis.Connection -> Handler CurrentBlock
getCurrentBlock dbConn redisConn = do
  let redisKey = "current_block_number"
  redisResult <- liftIO $ queryRedis redisConn redisKey
  case redisResult of
    Just result -> case eitherDecodeStrict' (BSL.toStrict result) of
      Right currentBlock -> return $ CurrentBlock currentBlock
      Left err -> do
        liftIO $ print err
        throwError err500 { errBody = "Error decoding current block from Redis" }
    Nothing -> do
      let query = "SELECT current_block_number FROM block_counter;"
      dbResult <- liftIO $ Postgres.query_ dbConn query
      case dbResult of
        [(CurrentBlock cb)] -> do
          _ <- liftIO $ cacheToRedis redisConn redisKey (encode cb)
          return $ CurrentBlock cb
        _ -> throwError err404 { errBody = "Current block not found" }

fetchRecentBlocks :: Postgres.Connection -> IO [BlockInfo]
fetchRecentBlocks dbConn = do
  let query = "SELECT block_number, previous_hash, block_hash, timestamp, nonce, transaction_count FROM blocks ORDER BY block_number DESC LIMIT 20;"
  rows <- Postgres.query_ dbConn query
  return $ map (\(bn, ph, bh, ts, nonce', txCount) -> BlockInfo bn ph bh ts nonce' [] txCount Nothing Nothing Nothing Nothing Nothing Nothing) rows

getRecentBlocks :: Postgres.Connection -> Redis.Connection -> Handler [BlockInfo]
getRecentBlocks dbConn redisConn = do
  let redisKey = "recent_blocks"
  redisResult :: Maybe [BC.ByteString] <- liftIO $ queryRedisList redisConn redisKey
  case redisResult of
    Just result ->
      case map decodeBlocks result of
        [] -> do -- todo: same as Nothing, make it better
          dbResult <-  liftIO $ fetchRecentBlocks dbConn
          case dbResult of
            [] -> throwError err404 { errBody = "Recent blocks not found" }
            recentBlocks -> do
              _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentBlocks)
              return recentBlocks
        list -> return list
    Nothing -> do
      dbResult <-  liftIO $ fetchRecentBlocks dbConn
      case dbResult of
        [] -> throwError err404 { errBody = "Recent blocks not found" }
        recentBlocks -> do
          _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentBlocks)
          return recentBlocks
  where
    decodeBlocks :: BC.ByteString -> BlockInfo
    decodeBlocks raw = case eitherDecodeStrict' raw of
      Left err -> error err -- TODO: check how to throw this error
      Right gfl -> gfl

fetchRecentTxns :: Postgres.Connection -> IO [TxInDb]
fetchRecentTxns dbConn = do
  let query = "SELECT transaction_id, block_number, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount,gas_fee, timestamp, status, validator_name, vote_percent FROM transactions ORDER BY block_number DESC LIMIT 20;"
  rows <- Postgres.query_ dbConn query
  return $ map (\(txId, blockNumber, txnHash, reqKey, fw, tw, fwb, twb, txa, gf, ts, status', vn, vp) ->
    TxInDb (Just txId) (Just blockNumber) (Just $ pack txnHash) reqKey fw tw (Just fwb) twb (Just txa) gf (formatToGMTString ts) (read status' :: TxnStatus) vn vp) rows

getRecentTxDetails :: Postgres.Connection -> Redis.Connection -> Handler [TxInDb]
getRecentTxDetails dbConn redisConn = do
  let redisKey = "recent_transactions"
  redisResult :: Maybe [BC.ByteString] <- liftIO $ queryRedisList redisConn redisKey
  case redisResult of
    Just result ->
      case map decodeTxns result of
        [] -> do -- todo: same as Nothing, make it better
          dbResult <-  liftIO $ fetchRecentTxns dbConn
          case dbResult of
            [] -> throwError err404 { errBody = "Recent Transactions not found" }
            recentTxns -> do
              _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentTxns)
              return recentTxns
        list -> return list
    Nothing -> do
      dbResult <-  liftIO $ fetchRecentTxns dbConn
      case dbResult of
        [] -> throwError err404 { errBody = "Recent Transactions not found" }
        recentTxns -> do
          _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentTxns)
          return recentTxns

decodeTxns :: BC.ByteString -> TxInDb
decodeTxns raw = case eitherDecodeStrict' raw of
  Left err -> error err -- TODO: check how to throw this error
  Right gfl -> gfl

fetchRecentSmartContracts :: Postgres.Connection -> IO [TxInDb]
fetchRecentSmartContracts dbConn = do
  let query = "SELECT transaction_id, block_number, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount,gas_fee, timestamp, status, validator_name, vote_percent FROM transactions WHERE request_key IS NOT NULL ORDER BY timestamp DESC LIMIT 20;"
  rows <- Postgres.query_ dbConn query
  return $ map (\(txId, blockNumber, txnHash, reqKey, fw, tw, fwb, twb, txa, gf, ts, status', vn, vp) ->
    TxInDb (Just txId) (Just blockNumber) (Just $ pack txnHash) reqKey fw tw (Just fwb) twb (Just txa) gf (formatToGMTString ts) (read status' :: TxnStatus) vn vp) rows

getRecentScDetails :: Postgres.Connection -> Redis.Connection -> Handler [TxInDb]
getRecentScDetails dbConn redisConn = do
  let redisKey = "recent_smart_contracts"
  redisResult :: Maybe [BC.ByteString] <- liftIO $ queryRedisList redisConn redisKey
  case redisResult of
    Just result ->
      case map decodeTxns result of
        [] -> do -- todo: same as Nothing, make it better
          dbResult <-  liftIO $ fetchRecentSmartContracts dbConn
          case dbResult of
            [] -> throwError err404 { errBody = "Recent Smart Contracts not found" }
            recentTxns -> do
              _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentTxns)
              return recentTxns
        list -> return list
    Nothing -> do
      dbResult <-  liftIO $ fetchRecentSmartContracts dbConn
      case dbResult of
        [] -> throwError err404 { errBody = "Recent Smart Contracts not found" }
        recentTxns -> do
          _ <- liftIO $ cacheToRedisList redisConn redisKey (encode <$> recentTxns)
          return recentTxns

fetchTransactionCountInTime :: Postgres.Connection -> UTCTime -> IO Int
fetchTransactionCountInTime dbConn cutoffTime = do
  let query = "SELECT COUNT(*) FROM transactions WHERE timestamp >= ?;"
  [Postgres.Only count] <- Postgres.query dbConn query (Postgres.Only cutoffTime)
  return count

getBlocksWithTime :: Postgres.Connection -> Redis.Connection -> Maybe NominalDiffTime -> Handler Int
getBlocksWithTime dbConn _ timeInSeconds = do
  now <- liftIO getCurrentTime
  let cutoffTime = maybe now (flip addUTCTime now . negate) timeInSeconds
  liftIO $ print cutoffTime
  liftIO $ fetchTransactionCountInTime dbConn cutoffTime

fetchWalletBalance :: Postgres.Connection -> String -> IO (Maybe Scientific)
fetchWalletBalance dbConn walletAddress = do
  let query = "SELECT balance FROM users WHERE public_key = ?;"
  result <- Postgres.query dbConn query (Postgres.Only walletAddress)
  return $ case result of
    [Postgres.Only balance] -> Just balance
    _ -> Nothing

checkBalance :: Postgres.Connection -> Redis.Connection -> Request -> Handler Scientific
checkBalance dbConn redisConn Request{..} = do
  case address of
    Nothing -> throwError $ err404 { errBody = "Wallet Address input not given" }
    Just walletAddr -> do
      let redisKey = BC.pack $ "wa:" ++ walletAddr
      redisResult <- liftIO $ queryRedis redisConn redisKey
      case redisResult of
        Just result ->
          case eitherDecodeStrict' (BSL.toStrict result) :: Either String Scientific of
            Right balance -> return balance
            Left err -> do
              liftIO $ putStrLn $ "Decoding error: " ++ err
              throwError err500 { errBody = "Error decoding current block from Redis" }
        Nothing -> do
          balance <- liftIO $ fetchWalletBalance dbConn walletAddr
          case balance of
            Just (b :: Scientific) -> do
              let encodedBalance = encode (realToFrac b :: Double)
              _ <- liftIO $ cacheToRedis redisConn redisKey encodedBalance
              return (read (show b) :: Scientific)
            Nothing -> throwError err404 { errBody = "Wallet not found" }

verifyWalletExists :: Postgres.Connection -> String -> IO Bool
verifyWalletExists dbConn walletAddress = do
  let query = "SELECT 1 FROM users WHERE public_key = ?;"
  result <- Postgres.query dbConn query (Postgres.Only walletAddress) :: IO [Postgres.Only Int]
  return $ not (null result)

verifyAddress :: Postgres.Connection -> Redis.Connection -> Request -> Handler Bool
verifyAddress dbConn redisConn Request{..} = do
  case address of
    Nothing -> throwError $ err404 { errBody = "Wallet Address input not given" }
    Just walletAddr -> do
      let redisKey = BC.pack $ "wa:" ++ walletAddr
      result <- liftIO $ Redis.runRedis redisConn $ Redis.exists redisKey
      case result of
        Right redisBool ->
          if redisBool
            then return redisBool
            else liftIO $ verifyWalletExists dbConn walletAddr
        Left _ -> liftIO $ verifyWalletExists dbConn walletAddr

getWalletInfo :: Postgres.Connection -> Redis.Connection -> Request -> Handler Address
getWalletInfo dbConn redisConn request = do
  case address request of
    Nothing -> throwError $ err404 { errBody = "Wallet Address input not given" }
    Just walletAddr -> do
      mBalance <- checkBalance dbConn redisConn request
      cachedResult <- liftIO $ Redis.runRedis redisConn $ Redis.lrange (BC.pack ("wa-tx:" ++ walletAddr)) 0 (-1)
      case cachedResult of
        Right [] -> fetchAndCacheFromDb dbConn redisConn walletAddr mBalance
        Left _ -> fetchAndCacheFromDb dbConn redisConn walletAddr mBalance
        Right transactions -> do
          let decodedTransactions = map (eitherDecodeStrict' @TxInDb) transactions
          case sequence decodedTransactions of
            Right txList -> returnAddressData txList walletAddr (Just mBalance)
            Left err -> throwError $ err400 { errBody = BSL.fromStrict $ BC.pack $ "Cache decode error: " ++ show err }
  where
    fetchAndCacheFromDb dbConn redisConn walletAddr mBalance = do
      dbResult <- liftIO $ fetchTransactionsForWallet dbConn walletAddr
      case dbResult of
        [] -> throwError $ err404 { errBody = "No Details found for this Wallet Address in db" }
        txList -> do
          liftIO $ mapM_ (Redis.runRedis redisConn . Redis.rpush (BC.pack ("wa-tx:" ++ walletAddr)) . pure . BSL.toStrict . encode) txList
          returnAddressData txList walletAddr (Just mBalance)

fetchTransactionsForWallet :: Postgres.Connection -> String -> IO [TxInDb]
fetchTransactionsForWallet dbConn walletAddr = do
  let query = "SELECT transaction_id, block_number, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount,gas_fee, timestamp, status, validator_name, vote_percent FROM transactions WHERE from_wallet = ? OR to_wallet = ? ORDER BY timestamp DESC;"
  rows <- Postgres.query dbConn query (walletAddr, walletAddr)
  return $ map (\(txId, blockNumber, txnHash, reqKey, fw, tw, fwb, twb, txa, gf, ts, status', vn, vp) ->
    TxInDb (Just txId) (Just blockNumber) (Just $ pack txnHash) reqKey fw tw (Just fwb) twb (Just txa) gf (formatToGMTString ts) (read status' :: TxnStatus) vn vp) rows

returnAddressData :: [TxInDb] -> String -> Maybe Scientific -> Handler Address
returnAddressData txList walletAddr mBalance =
  let (lts, fts) =
        if null txList
          then (Nothing, Nothing)
          else (transaction_hash $ last txList, transaction_hash $ head txList)
  in return Address
    { addressW = walletAddr
    , balance = mBalance
    , value = Nothing
    , last_transaction_sent = lts
    , first_transaction_sent = fts
    , transactions = Just txList
    , details = Nothing
    }

getDetails :: Postgres.Connection -> Redis.Connection -> Handler BlockDetails
getDetails dbConn redisConn = do
  CurrentBlock cb <- getCurrentBlock dbConn redisConn
  totalTransactionsRedis <- do
    redisResult <- liftIO $ Redis.runRedis redisConn $ Redis.get "transaction_counter"
    return $ case redisResult of
      Right (Just value) -> Just (read (BC.unpack value) :: Int)
      _ -> Nothing
  txnCount <-
    case totalTransactionsRedis of
      Nothing -> do
        let query = "SELECT COUNT(*) FROM transactions;"
        [Postgres.Only count] :: [Postgres.Only Int] <- liftIO $ Postgres.query_ dbConn query
        _ <- liftIO $ cacheToRedis redisConn "transaction_counter" (encode count)
        return count
      Just n -> return n
  totalSmartContractsRedis <- do
    redisResult <- liftIO $ Redis.runRedis redisConn $ Redis.get "smart_contract_counter"
    return $ case redisResult of
      Right (Just value) -> Just (read (BC.unpack value) :: Int)
      _ -> Nothing
  scCount <-
    case totalSmartContractsRedis of
      Nothing -> do
        let query = "SELECT COUNT(*) FROM transactions WHERE request_key IS NOT NULL;"
        [Postgres.Only count] :: [Postgres.Only Int] <- liftIO $ Postgres.query_ dbConn query
        _ <- liftIO $ cacheToRedis redisConn "smart_contract_counter" (encode count)
        return count
      Just n -> return n
  return $ BlockDetails (cb + 1) txnCount scCount