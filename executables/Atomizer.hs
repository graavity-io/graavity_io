{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Codec.Winery
import Data.Aeson hiding (Key, Value)
import GHC.Generics

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash (Digest, SHA256, hash)

import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as DT
import Data.Text.Encoding

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.Redis as Redis
import qualified Kafka.Producer as Kafka

import Pact.Types.Command as P
import qualified Pact.Types.Hash as P

import Network.RPC.Curryer.Server
import Pact.JSON.Encode (toJsonViaEncode)

import System.Environment
import System.Random
import Data.Scientific
import Servant hiding (serve)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Time (UTCTime, getCurrentTime)

data Log =
  GasFee GasFeeLog
  | Pact [PactLog]
  deriving (Generic, FromJSON)
  deriving Serialise via WineryVariant Log

data GasFeeLog = GasFeeLog
 { pactRequest :: Maybe DT.Text --Maybe (P.Command DT.Text)
 , userKey :: String
 , requestKeys :: Maybe DT.Text --Maybe (P.RequestKeys)
 , gasFee :: Scientific
 , txnDetails :: TxnDetails
 , txnHash :: Maybe DT.Text
 , blockNum :: Maybe Int
 , validatorName :: Maybe String
 , votePercent :: Maybe Float
 }
 deriving (Generic, Show, ToJSON, FromJSON)
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
  deriving (Generic, FromJSON, Show, ToJSON)
  deriving Serialise via WineryVariant TxnDetails

data TxnStatus =
  SUCCESS | FAILED | PENDING
  deriving (Generic, FromJSON, Show, ToJSON, Read)
  deriving Serialise via WineryVariant TxnStatus

newtype PactLog = PactLog
  { result :: DT.Text -- P.CommandResult Hash
  }
  deriving (Generic, FromJSON)
  deriving Serialise via WineryVariant PactLog

instance ToJSON (CommandResult P.Hash) where
  toJSON = toJsonViaEncode

data Block = Block
  { blockNumber :: Int
  , previousHash :: DT.Text
  , timestamp :: UTCTime
  , transactions :: [TxInDb]
  , transactionsCount :: Int
  , nonce :: Int
  , blockHash :: DT.Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data TxInDb =
  TxInDb
    { transaction_id :: Maybe Int
    , block_number :: Maybe Int
    , transaction_hash :: Maybe DT.Text
    , request_key :: Maybe DT.Text
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

gasFeeLogToTxInDb :: GasFeeLog -> TxnDetails -> TxInDb
gasFeeLogToTxInDb gfl@GasFeeLog{..} td@TxnDetails{..} =
  TxInDb txnId blockNum txnHash requestKeys fromKey toKey fromKeyAmt toKeyAmt txnAmt gasFee txnTimestamp txnStatus validatorName votePercent

initRedis :: IO Redis.Connection
initRedis = do
  redisHost <- fromMaybe "localhost" <$> lookupEnv "REDIS_HOST"
  redisPort <- fromMaybe "6379" <$> lookupEnv "REDIS_PORT"
  redisPassword <- lookupEnv "REDIS_PASSWORD"
  let connectInfo = Redis.defaultConnectInfo {
        Redis.connectHost = redisHost,
        Redis.connectPort = Redis.PortNumber (read redisPort),
        Redis.connectAuth = fmap Char.pack redisPassword
      }
  Redis.checkedConnect connectInfo

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

initGenesisBlock :: IO Block
initGenesisBlock = do
  currentTime <- getCurrentTime
  let blockHashValue = calculateBlockHash 0 (DT.replicate 64 "0") currentTime [] 0
  return Block
    { blockNumber = 0
    , previousHash = DT.replicate 64 "0"
    , timestamp = currentTime
    , transactions = []
    , transactionsCount = 0
    , nonce = 0
    , blockHash = blockHashValue
    }

storeGenesisBlock :: Postgres.Connection -> Redis.Connection -> IO Block
storeGenesisBlock dbConn redisConn = do
  genesisBlock <- initGenesisBlock
  insertBlockIntoDB dbConn genesisBlock
  cacheBlockInRedis redisConn genesisBlock
  cacheRecentBlock redisConn genesisBlock
  return genesisBlock

fetchLastBlock :: Postgres.Connection -> Redis.Connection -> IO Block
fetchLastBlock dbConn redisConn = do
  redisResult <- Redis.runRedis redisConn $ Redis.get "current_block_number"
  case redisResult of
    Right (Just blockNumStr) -> do
      let blockKey = "block:" <> blockNumStr
      blockResult <- Redis.runRedis redisConn $ Redis.get blockKey
      case blockResult of
        Right (Just blockData) ->
          case eitherDecodeStrict' blockData of
            Right block -> return block
            Left err    -> error $ "Error decoding block from Redis: " ++ err
        _ -> fetchFromDBAndCache dbConn redisConn (read $ Char.unpack blockNumStr :: Int)
    _ -> do
      lastBlock <- fetchLastBlockFromDB dbConn
      maybe (storeGenesisBlock dbConn redisConn) return lastBlock

fetchFromDBAndCache :: Postgres.Connection -> Redis.Connection -> Int -> IO Block
fetchFromDBAndCache dbConn redisConn blockNumber = do
  let query = "SELECT block_number, previous_hash, timestamp, nonce, block_hash, transaction_count FROM blocks WHERE block_number = ?;"
  result <- Postgres.query dbConn query (Postgres.Only blockNumber)
  case result of
    [(blockNumber, previousHash, timestamp, nonce, blockHash, txCount)] -> do
      let block = Block
            { blockNumber = blockNumber
            , previousHash = previousHash
            , timestamp = timestamp
            , transactions = []
            , transactionsCount = txCount
            , nonce = nonce
            , blockHash = blockHash
            }
      cacheBlockInRedis redisConn block
      cacheRecentBlock redisConn block
      return block
    _ -> error "Block not found in database"

fetchLastBlockFromDB :: Postgres.Connection -> IO (Maybe Block)
fetchLastBlockFromDB dbConn = do
  let query = "SELECT block_number, previous_hash, timestamp, nonce, block_hash, transaction_count FROM blocks ORDER BY block_number DESC LIMIT 1;"
  result <- Postgres.query_ dbConn query
  case result of
    [(blockNumber, previousHash, timestamp, nonce, blockHash, txCount)] -> 
      return $ Just Block
        { blockNumber = blockNumber
        , previousHash = previousHash
        , timestamp = timestamp
        , transactions = []
        , transactionsCount = txCount
        , nonce = nonce
        , blockHash = blockHash
        }
    _ -> return Nothing

producerProps :: Kafka.ProducerProperties
producerProps = Kafka.brokersList ["localhost:9092"] <> Kafka.logLevel Kafka.KafkaLogInfo

updateTxList :: Block -> Block
updateTxList block@Block{..} = Block blockNumber previousHash timestamp [] transactionsCount nonce blockHash

main  :: IO ()
main = do
  -- _ <- loadFile defaultConfig
  dbConn <- initPostgres
  redisConn <- initRedis
  var <- newChan :: IO (Chan DT.Text)
  lastBlock <- fetchLastBlock dbConn redisConn
  kafkaProducer <- Kafka.newProducer producerProps
  case kafkaProducer of
    Left err -> putStrLn $ "Failed to create Kafka Producer: " ++ show err
    Right producer -> do
      txBuffer <- newMVar []  -- Buffer to hold transactions for the next block
      _ <- forkIO $ runAtomizer txBuffer var
      _ <- forkIO $ automaticBlockGenerator dbConn redisConn producer lastBlock txBuffer
      putStrLn "starting atomizer RPC server..."
      -- modify allHostAddrs to only shard and pact address
      void $ serve userRequestHandlers var allHostAddrs 8569 Nothing

runAtomizer :: MVar [TxInDb] -> Chan DT.Text -> IO ()
runAtomizer txBuffer txChan = forever $ do
  txData <- readChan txChan
  case eitherDecodeStrict' (encodeUtf8 txData) :: Either String Log of
    Right (Pact pl) ->
      -- write the pact status against the requestKey
      mapM_ (\(PactLog r) ->
        case eitherDecodeStrict' (encodeUtf8 r) :: Either String (P.CommandResult P.Hash) of
          Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ Char.pack $ show err }
          Right result -> return () -- putWithCounter (Char.pack ("RK" ++ DT.unpack (P.requestKeyToB16Text (P._crReqKey result)))) (BSL.toStrict $ encode result)
          ) pl
          -- append log only file with gas fee txns.
    Right (GasFee gfl@GasFeeLog{..}) -> do
      tId <- liftIO $ generateTransactionId
      let updatedTxn = TxnDetails (Just tId) (fromKey txnDetails) (fromKeyAmt txnDetails) (toKey txnDetails) (toKeyAmt txnDetails) (txnAmt txnDetails) (txnTimestamp txnDetails) (txnStatus txnDetails)
          updatedGfl = GasFeeLog pactRequest userKey requestKeys gasFee updatedTxn txnHash blockNum Nothing Nothing -- todo: validator name and vote percent field from validator
          txInDb = gasFeeLogToTxInDb updatedGfl updatedTxn
      modifyMVar_ txBuffer $ \txs -> return (txInDb : txs)
          -- msg = BSL.toStrict $ encode updatedGfl
      -- newBlock <- generateNewBlock updatedGfl lastBlock
      -- sendMessageToKafka producer msg
      -- -- insertTransaction dbConn redisConn updatedGfl
      -- insertBlockIntoDB dbConn newBlock
      -- cacheBlockInRedis redisConn newBlock
      -- putStrLn $ "New block generated: " ++ show newBlock
    Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ Char.pack $ ("Error decoding transaction: " ++ show err) }

transactionTopic :: Kafka.TopicName
transactionTopic = Kafka.TopicName "transaction-log"

sendMessageToKafka :: Kafka.KafkaProducer -> Char.ByteString -> IO ()
sendMessageToKafka producer msg = do
  let record = Kafka.ProducerRecord transactionTopic Kafka.UnassignedPartition (Just "Atomizer") (Just msg)
  res <- Kafka.produceMessage producer record
  case res of
    Just err -> putStrLn $ "Error producing message: " ++ show err
    Nothing  -> putStrLn "Message sent successfully to Kafka topic `transaction-log`."

calculateBlockHash :: Int -> DT.Text -> UTCTime -> [TxInDb] -> Int -> DT.Text
calculateBlockHash blockNum prevHash time txs nonce =
  let blockData = Char.pack $ show blockNum ++ show prevHash ++ show time ++ show txs ++ show nonce
  in DT.pack . show $ (hash blockData :: Digest SHA256)

-- Generates a new block every 300 ms, including all buffered transactions
automaticBlockGenerator :: Postgres.Connection -> Redis.Connection -> Kafka.KafkaProducer -> Block -> MVar [TxInDb] -> IO ()
automaticBlockGenerator dbConn redisConn producer lastBlock txBuffer = forever $ do
  threadDelay 300000
  txs <- swapMVar txBuffer [] -- Get and reset the buffer
  updatedLastBlock <- fetchLastBlock dbConn redisConn
  newBlock <- generateNewBlock dbConn redisConn txs updatedLastBlock
  insertBlockAndCache dbConn redisConn producer newBlock

generateNewBlock :: Postgres.Connection -> Redis.Connection -> [TxInDb] -> Block -> IO Block
generateNewBlock dbConn redisConn txs lastBlock = do
  currentTime <- getCurrentTime
  newBlockNumber <- incrementBlockNumber dbConn redisConn
  let updatedTxs = map (updateTx newBlockNumber) txs
      prevHash = blockHash lastBlock
      nonce = 0
      blockHashValue = calculateBlockHash newBlockNumber prevHash currentTime updatedTxs nonce
  return Block
    { blockNumber = newBlockNumber
    , previousHash = prevHash
    , timestamp = currentTime
    , transactions = updatedTxs
    , transactionsCount = length updatedTxs
    , nonce = nonce
    , blockHash = blockHashValue
    }

incrementBlockNumber :: Postgres.Connection -> Redis.Connection -> IO Int
incrementBlockNumber dbConn redisConn = do
  let query = "UPDATE block_counter SET current_block_number = current_block_number + 1 RETURNING current_block_number;"
  [Postgres.Only newBlockNumber] <- Postgres.query_ dbConn query
  _ <- Redis.runRedis redisConn $ Redis.set "current_block_number" (Char.pack $ show newBlockNumber)
  return newBlockNumber

insertBlockAndCache :: Postgres.Connection -> Redis.Connection -> Kafka.KafkaProducer -> Block -> IO ()
insertBlockAndCache dbConn redisConn producer block = do
  insertBlockIntoDB dbConn block
  mapM_ (insertTransaction dbConn redisConn (blockNumber block)) (transactions block)
  cacheBlockInRedis redisConn block
  updateRecentBlocks redisConn (updateTxList block)
  sendBlockToKafka producer block

updateTx :: Int -> TxInDb -> TxInDb
updateTx blockNo txInDb@TxInDb{..} =
  TxInDb transaction_id (Just blockNo) transaction_hash request_key from_wallet to_wallet from_wallet_balance to_wallet_balance transaction_amount gas_fee timestamp status validator_name vote_percent

insertTransaction :: Postgres.Connection -> Redis.Connection -> Int -> TxInDb -> IO ()
insertTransaction dbConn redisConn blockNumber txn@TxInDb{..} = do
  txId <- generateTransactionId
  let query = "INSERT INTO transactions (transaction_id, block_number, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount, gas_fee, timestamp, status, validator_name, vote_percent) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
  _ <- Postgres.execute dbConn query
    ( transaction_id
    , block_number
    , transaction_hash
    , request_key
    , from_wallet
    , to_wallet
    , from_wallet_balance
    , to_wallet_balance
    , transaction_amount
    , gas_fee
    , timestamp
    , show status
    , validator_name
    , vote_percent
    )
  cacheTransactionDetails redisConn txn
  cacheRecentTransactions redisConn txn
  cacheRecentSmartContracts redisConn txn
  cacheWalletTransactions redisConn from_wallet txn
  when (isJust to_wallet) $ cacheWalletTransactions redisConn (fromJust to_wallet) txn
  when (isJust request_key) $ cacheRequestKey redisConn (fromJust (DT.unpack <$> request_key)) txn

cacheTransactionDetails :: Redis.Connection -> TxInDb -> IO ()
cacheTransactionDetails redisConn txn@TxInDb{..} = do
  let txnKey = "txn:" <> Char.pack (DT.unpack $ fromMaybe "" transaction_hash)
  _ <- Redis.runRedis redisConn $ Redis.set txnKey (BSL.toStrict $ encode txn)
  return ()

cacheRecentTransactions :: Redis.Connection -> TxInDb -> IO ()
cacheRecentTransactions redisConn txn = do
  let encodedTxn = BSL.toStrict $ encode txn
  _ <- Redis.runRedis redisConn $ Redis.lpush "recent_transactions" [encodedTxn]
  _ <- Redis.runRedis redisConn $ Redis.ltrim "recent_transactions" 0 19 -- Keep only the last 20 transactions
  return ()

cacheRecentSmartContracts :: Redis.Connection -> TxInDb -> IO ()
cacheRecentSmartContracts redisConn txn = do
  let encodedTxn = BSL.toStrict $ encode txn
  _ <- Redis.runRedis redisConn $ Redis.lpush "recent_smart_contracts" [encodedTxn]
  _ <- Redis.runRedis redisConn $ Redis.ltrim "recent_smart_contracts" 0 19 -- Keep only the last 20 smart contracts
  return ()

cacheWalletTransactions :: Redis.Connection -> String -> TxInDb -> IO ()
cacheWalletTransactions redisConn wallet txn = do
  let walletKey = "wa-tx:" <> Char.pack wallet
  _ <- Redis.runRedis redisConn $ Redis.rpush walletKey [BSL.toStrict $ encode txn]
  -- _ <- Redis.runRedis redisConn $ Redis.ltrim walletKey 0 19 -- todo: check if we need to Keep only the last 20 transactions
  return ()

cacheRecentBlock :: Redis.Connection -> Block -> IO ()
cacheRecentBlock redisConn block = do
  _ <- Redis.runRedis redisConn $ Redis.rpush "recent_blocks" [BSL.toStrict $ encode (updateTxList block)]
  _ <- Redis.runRedis redisConn $ Redis.ltrim "recent_blocks" 0 19 -- Keep only the last 20 transactions
  return ()

cacheRequestKey :: Redis.Connection -> String -> TxInDb -> IO ()
cacheRequestKey redisConn reqKey txn = do
  let key = "reqkey:" <> Char.pack reqKey
  _ <- Redis.runRedis redisConn $ Redis.set key (BSL.toStrict $ encode txn)
  return ()

cacheBlockDetails :: Redis.Connection -> Block -> IO ()
cacheBlockDetails redisConn block@Block{..} = do
  let blockKey = "block:" <> Char.pack (show blockNumber)
  _ <- Redis.runRedis redisConn $ Redis.set blockKey (BSL.toStrict $ encode block)
  return ()

updateRecentBlocks :: Redis.Connection -> Block -> IO ()
updateRecentBlocks redisConn block@Block{..} = do
  let encodedBlock = BSL.toStrict $ encode block
  _ <- Redis.runRedis redisConn $ Redis.lpush "recent_blocks" [encodedBlock]
  _ <- Redis.runRedis redisConn $ Redis.ltrim "recent_blocks" 0 19
  return ()

sendBlockToKafka :: Kafka.KafkaProducer -> Block -> IO ()
sendBlockToKafka producer block = do
  let msg = BSL.toStrict $ encode block
      record = Kafka.ProducerRecord transactionTopic Kafka.UnassignedPartition (Just "Atomizer") (Just msg)
  res <- Kafka.produceMessage producer record
  case res of
    Just err -> putStrLn $ "Error producing message: " ++ show err
    Nothing  -> putStrLn "Block sent successfully to Kafka topic `transaction-log`."
  -- Kafka.flushProducer producer (Kafka.Timeout 10000)

insertBlockIntoDB :: Postgres.Connection -> Block -> IO ()
insertBlockIntoDB dbConn Block{..} = do
  let query = "INSERT INTO blocks (block_number, previous_hash, timestamp, transaction_count, nonce, block_hash) VALUES (?, ?, ?, ?, ?, ?);"
  _ <- Postgres.execute dbConn query (blockNumber, previousHash, timestamp, length transactions, nonce, blockHash)
  return ()

cacheBlockInRedis :: Redis.Connection -> Block -> IO ()
cacheBlockInRedis redisConn block@Block{..} = do
  let key = "block:" <> Char.pack (show blockNumber)
  _ <- Redis.runRedis redisConn $ Redis.set key (BSL.toStrict $ encode block)
  return ()

generateTransactionId :: IO Int
generateTransactionId = randomRIO (10 ^ (10 :: Int), (10 ^ (15 :: Int)) - 1)

-- setup incoming request handlers to operate on the server's state
userRequestHandlers :: RequestHandlers (Chan DT.Text)
userRequestHandlers =
  [ RequestHandler $ \state req -> do
      writeChan (connectionServerState state) req
      return req
  ]
-----------------------------------------------------------------------------------------------
-- insertTransaction :: Postgres.Connection -> Redis.Connection -> GasFeeLog -> IO ()
-- insertTransaction dbConn redisConn updatedGfl@GasFeeLog{..} = do
--   let TxnDetails {..} = txnDetails
--   let query = "INSERT INTO transactions (block_number, transaction_id, transaction_hash, request_key, from_wallet, to_wallet, from_wallet_balance, to_wallet_balance, transaction_amount, gas_fee, timestamp, status, validator_name, vote_percent) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"
--   _ <- Postgres.execute dbConn query
--     (blockNum, txnId, txnHash, requestKeys, fromKey, toKey, fromKeyAmt, toKeyAmt, txnAmt, gasFee, txnTimestamp, show txnStatus, validatorName, votePercent)
--   _ <- incrementCounterInRedis redisConn "transaction_counter"
--   case (txnId, txnHash, blockNum) of
--     (Just tId, Just txHash, Just bn) -> do
--       cacheTransactionInRedis redisConn (Char.pack $ "tx:" ++ show tId) updatedGfl
--       cacheTransactionInRedis redisConn (Char.pack $ "th:" <> DT.unpack txHash) updatedGfl
--       cacheTransactionInRedis redisConn (Char.pack $ "bn:" ++ show bn) updatedGfl
--       addTransactionToRedisList redisConn "recent_transactions" updatedGfl
--     _ -> return ()
--   case requestKeys of
--     Just rk -> do
--       cacheTransactionInRedis redisConn (Char.pack $ "rk:" <> DT.unpack rk) updatedGfl
--       incrementCounterInRedis redisConn "smart_contract_counter"
--       addTransactionToRedisList redisConn "recent_smartcontracts" updatedGfl
--     Nothing -> return ()
--   addTransactionToWalletHistory redisConn fromKey updatedGfl
--   case toKey of
--     Nothing -> return ()
--     Just tk -> addTransactionToWalletHistory redisConn tk updatedGfl
--   return ()

generateBlockNumber' :: IO Int
generateBlockNumber' = do
  gen <- getStdGen
  let (randNum, _) = randomR (10 ^ (4 :: Int), (10 ^ (9 :: Int)) - 1) gen
  return randNum

generateBlockNumber :: Postgres.Connection -> Redis.Connection -> IO Int
generateBlockNumber dbConn redisConn = do
  let redisKey = "current_block_number"  
  redisResult <- Redis.runRedis redisConn $ Redis.get redisKey
  newBlockNumber <- case redisResult of
    Right (Just blockStr) -> do
      let existingBlockNumber = read (Char.unpack blockStr) :: Int
      dbCurrentBlock <- getCurrentBlockNumber dbConn
      case dbCurrentBlock of
        Just _ -> incrementBlockNumber' dbConn
        Nothing -> do
          initializeBlockCounter dbConn existingBlockNumber
          incrementBlockNumber' dbConn    
    _ -> do
      currentBlock <- getCurrentBlockNumber dbConn
      case currentBlock of
        Nothing -> do
          randomBlock <- generateBlockNumber'
          initializeBlockCounter dbConn randomBlock
          _ <- Redis.runRedis redisConn $ Redis.set redisKey (Char.pack $ show randomBlock)
          return randomBlock
        Just _ -> do
          newBlockNumber <- incrementBlockNumber' dbConn
          _ <- Redis.runRedis redisConn $ Redis.set redisKey (Char.pack $ show newBlockNumber)
          return newBlockNumber
  _ <- Redis.runRedis redisConn $ Redis.set redisKey (Char.pack $ show newBlockNumber)
  return newBlockNumber

getCurrentBlockNumber :: Postgres.Connection -> IO (Maybe Int)
getCurrentBlockNumber dbConn = do
  let query = "SELECT current_block_number FROM block_counter LIMIT 1;"
  result <- Postgres.query_ dbConn query
  case result of
    [Postgres.Only blockNumber] -> return (Just blockNumber)
    _ -> return Nothing

incrementBlockNumber' :: Postgres.Connection -> IO Int
incrementBlockNumber' dbConn = do
  let query = "UPDATE block_counter SET current_block_number = current_block_number + 1 RETURNING current_block_number;"
  [Postgres.Only newBlockNumber] <- Postgres.query_ dbConn query
  return newBlockNumber

initializeBlockCounter :: Postgres.Connection -> Int -> IO ()
initializeBlockCounter dbConn initialBlockNumber = do
  let query = "INSERT INTO block_counter (current_block_number) VALUES (?) ON CONFLICT DO NOTHING;"
  _ <- Postgres.execute dbConn query (Postgres.Only initialBlockNumber)
  return ()

addTransactionToRedisList :: Redis.Connection -> String -> GasFeeLog -> IO ()
addTransactionToRedisList redisConn listKey transaction = do
  let encodedTransaction = BSL.toStrict $ encode transaction
  _ <- Redis.runRedis redisConn $ Redis.rpush (Char.pack listKey) [encodedTransaction]
  _ <- Redis.runRedis redisConn $ Redis.ltrim (Char.pack listKey) (-20) (-1)
  return ()

cacheTransactionInRedis :: Redis.Connection -> Char.ByteString -> GasFeeLog -> IO ()
cacheTransactionInRedis redisConn redisKey transaction = do
  _ <- Redis.runRedis redisConn $ Redis.set redisKey (BSL.toStrict $ encode transaction)
  _ <- Redis.runRedis redisConn $ Redis.expire redisKey (24 * 60 * 60)
  return ()

incrementCounterInRedis :: Redis.Connection -> String -> IO ()
incrementCounterInRedis redisConn counterKey = do
  result <- Redis.runRedis redisConn $ Redis.incr (Char.pack counterKey)
  case result of
    Right newValue -> putStrLn $ "Counter updated. New value: " ++ show newValue
    Left err -> putStrLn $ "Error incrementing counter in Redis: " ++ show err
  return ()

addTransactionToWalletHistory :: Redis.Connection -> String -> GasFeeLog -> IO ()
addTransactionToWalletHistory redisConn walletAddr transaction = do
  let walletKey = "wallet:" ++ walletAddr
  let encodedTransaction = BSL.toStrict $ encode transaction
  _ <- Redis.runRedis redisConn $ Redis.rpush (Char.pack walletKey) [encodedTransaction]
  return ()