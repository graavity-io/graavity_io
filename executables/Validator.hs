module Main where

import System.Environment (getArgs)
import Raft.Config (StorageState (..))
import graavity.Consensus.Raft.Node (LogStorage (..), nodeMain)
import qualified Data.ByteString.Char8 as Char8

main :: IO ()
main = do
  args <- getArgs
  case args of
    [currentNode, "new", otherNodes] ->
      nodeMain New FileStore (Char8.pack currentNode) (Char8.pack <$> (words otherNodes))
    [currentNode, "existing", otherNodes] ->
      nodeMain Existing FileStore (Char8.pack currentNode) (Char8.pack <$> (words otherNodes))
    _ -> putStrLn ("Usage: cabal run validator \"localhost:3000\" \"new\" \"localhost:3001 localhost:3002\"")

-- -- Example function that the leader node can call to handle a user creation request
-- handleCreateUserRequest :: Var -> Natural -> RaftSocketClientM Store UserCmd (Either Text (ClientWriteResp Store UserCmd))
-- handleCreateUserRequest publicKey initialAmount = do
--   -- Step 1: Validate the creation of the user by issuing ValidateCreateUser
--   validateResponses <- socketClientWrite (ValidateCreateUser publicKey)
  
--   -- Step 2: Count votes (e.g., 51% or more "yes" responses)
--   let positiveVotes = countPositiveVotes validateResponses
--   let totalNodes = length validateResponses
--   let requiredVotes = totalNodes `div` 2 + 1

--   -- Step 3: If majority approves, commit the change
--   if positiveVotes >= requiredVotes
--     then socketClientWrite (CreateUser publicKey initialAmount)  -- Commit the change
--     else return $ Left "Not enough votes to create the user"

-- -- Similarly, handle the transfer of funds
-- handleTransferFundsRequest :: Var -> Var -> Natural -> RaftSocketClientM Store UserCmd (Either Text (ClientWriteResp Store UserCmd))
-- handleTransferFundsRequest fromKey toKey amount = do
--   -- Step 1: Validate the transfer
--   validateResponses <- socketClientWrite (ValidateTransferFunds fromKey toKey amount)
  
--   -- Step 2: Count votes
--   let positiveVotes = countPositiveVotes validateResponses
--   let totalNodes = length validateResponses
--   let requiredVotes = totalNodes `div` 2 + 1

--   -- Step 3: If majority approves, commit the transfer
--   if positiveVotes >= requiredVotes
--     then socketClientWrite (TransferFunds fromKey toKey amount)  -- Commit the change
--     else return $ Left "Not enough votes to transfer the funds"

-- -- Helper function to count positive votes
-- countPositiveVotes :: [ClientWriteResp Store UserCmd] -> Int
-- countPositiveVotes responses = length (filter isSuccess responses)

-- isSuccess :: ClientWriteResp Store UserCmd -> Bool
-- isSuccess (Right _) = True
-- isSuccess (Left _)  = False


-- parseNodes :: P.String -> [P.String]
-- parseNodes nodes = map toS (words nodes)

-- initRaftNode :: P.String -> [P.String] -> IO ()
-- initRaftNode myNodeIp fellowNodes = do
--   persistentState <- loadPersistentState "validatorDB"
--   let config = RaftNodeConfig
--                 { raftConfigNodeId = Char8.pack myNodeIp
--                 , raftConfigNodeIds = Set.fromList (Char8.pack <$> fellowNodes)
--                 , raftConfigElectionTimeout = (150, 300)
--                 , raftConfigHeartbeatTimeout = 100
--                 , raftConfigStorageState = Existing
--                 }
--   startRaftNode config persistentState

-- startRaftNode :: RaftNodeConfig -> PersistentState -> IO ()
-- startRaftNode config persistentState = do
--   let nodeState = initRaftNodeState
--   runRaftNode config nodeState persistentState

-- loadPersistentState :: FilePath -> IO PersistentState
-- loadPersistentState path = do
--   DLH.runLevelDB path DLH.def { DLH.createIfMissing = True } $ do
--     persistentState <- DLH.get "persistentState"
--     case persistentState of
--       Just ps -> pure $ decodeStrict' ps
--       Nothing -> do
--         let newState = initPersistentState
--         DLH.put "persistentState" (encode newState)
--         pure newState

-- addNewNode :: NodeId -> RaftNodeState v -> TransitionM sm v ()
-- addNewNode newNode nodeState = do
--   let leaderId = getLeader nodeState
--   send leaderId (AddNodeRPC newNode)

-- synchronizeNewNode :: NodeId -> TransitionM sm v ()
-- synchronizeNewNode newNode = do
--   logEntries <- getRaftLog
--   send newNode (SyncLogRPC logEntries)

-- persistRaftLogs :: Entries v -> IO ()
-- persistRaftLogs entries = DLH.runLevelDB "/validatorNodeDB" DLH.def { DLH.createIfMissing = True } $ do
--   mapM_ (\entry -> DLH.put (pack $ show $ entryIndex entry) (encode entry)) entries

-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RecordWildCards #-}

-- module Main where

-- import Codec.Winery
-- import Control.Concurrent.STM.TChan
-- import Control.Monad.STM
-- import Control.Concurrent
-- import Control.Concurrent.Async
-- import Control.Monad
-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.Aeson
-- import Data.Either
-- import Data.Serialize
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.HashMap.Strict as HM
-- import Data.List.NonEmpty as NE (head)
-- import Data.Scientific
-- import Data.Text (Text)
-- import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import Database.LevelDB.Higher
-- import GHC.Generics
-- import Network.RPC.Curryer.Client
-- import Network.RPC.Curryer.Server
-- import Network.Socket hiding (connect)
-- import graavity.Config.TMVar
-- import graavity.Consensus.Publish
-- import graavity.Consensus.Raft.Node
-- import Raft.Config
-- import Raft.Client
-- import graavity.Consensus.Raft.Common
-- import graavity.Consensus.Raft.SocketNode
-- import Raft.Types
-- import Raft
-- import graavity.HTTP.ApiServer hiding (sendClient)
-- import graavity.Spec.Simple
-- import qualified Pact.Types.API as P
-- import Pact.Types.Command as P
-- import Pact.Types.Hash as P
-- import System.Environment
-- import Text.Read (readMaybe)

-- main :: IO ()
-- main = do
--   [x] <- getArgs
--   let currentNode = BC.pack x
--   let peers = filter (/= currentNode) ["localhost:9000", "localhost:9001", "localhost:9002"]

--   -- Initialize the socket environment and start receiving client requests in a separate thread
--   asyncClientListener <- async $ do
--     clientPort <- getFreePort
--     let clientHost = "localhost"
--         clientId = ClientId $ hostPortToNid (clientHost, show clientPort)
--     nodeSockEnv :: NodeSocketEnv Text UserRequest <- initSocketEnv currentNode
--     if currentNode == "localhost:9001"
--       then do
--         print currentNode
--         req :: Either Text (ClientRequest UserRequest) <- runRaftSocketT nodeSockEnv receiveClient
--         print "----------Req----------"
--         case req of
--           Left err -> do
--             print "coming to left err"
--             print ("Err ----> " <> err)
--           Right (ClientRequest _ (ClientWriteReq (ClientCmdReq _ uc)))  -> do
--             print "coming to right uc"
--             print "UC -----> "
--             print uc

            -- DLH.runLevelDB "/sharddb" def{createIfMissing=True} (def, def {sync = True}) "ShardSpace" $
            --   put (pack $ signedUserPublicKey uc) (pack $ show $ initialAmount uc)
            
--         let y :: ClientResponse Text UserRequest = (ClientRedirectResponse (ClientRedirResp NoLeader))
--         runRaftSocketT nodeSockEnv $ sendClient clientId y --(ClientRedirectResponse (ClientRedirResp NoLeader))
--       else undefined

--   -- Run the Raft node in the main thread
--   nodeMain New FileStore currentNode peers

--   -- Wait for the async client listener to complete (which might be forever in this context)
--   void $ wait asyncClientListener

-- data UserRequest =
--   UserRequest
--     { signedUserPublicKey :: String
--     , initialAmount :: Float
--     , hash :: Text
--     }
--   deriving (Generic, Show, Eq, FromJSON, ToJSON, Serialize)
--   deriving Serialise via WineryVariant UserRequest

-- instance RaftStateMachinePure Text UserRequest where
--   data RaftStateMachinePureError Text UserRequest = SomeError [Char]
--    deriving (Show, Generic, Serialize)


-- instance RaftRecvClient IO UserRequest where
--   type RaftRecvClientError IO UserRequest = Text
--   receiveClient = do
    -- cReq :: TChan (ClientRequest UserRequest) <- <$> atomically newTChan
    -- cReq :: TChan (ClientRequest UserRequest) <- asks nsClientReqQueue
    -- NodeSocketEnv{..} <- ask
    -- nodeSockEnv <- initSocketEnv "localhost:9000"
    -- fmap Right . liftIO . atomically $ readTChan nsClientReqQueue
    -- let clientHost = "localhost"
    -- clientPort <- getFreePort
    -- let clientId = ClientId $ hostPortToNid (clientHost, show clientPort)
    -- let ur = UserRequest "" 10.0 "" 
    -- return (Right (ClientRequest clientId (ClientWriteReq (ClientCmdReq (SerialNum 1) ur)))) -- :: m (Either (RaftRecvClientError m v) (ClientRequest v))


-- data ValidatorRequest
--   = FromSentinel SCRequest
--   | FromNode Text 
--   deriving (Generic, Show)

-- data SCRequest =
--   SCRequest
--     { code :: Text -- P.Command (P.Payload P.PrivateMeta P.ParsedCode)
--     , publicKey :: String
--     , key :: Text -- P.RequestKeys
--     , originalReq :: Text -- P.Command Text
--     , idx :: Maybe Int
--     }
--   deriving (Generic, Show)
--   deriving Serialise via WineryVariant SCRequest

-- data ValidatorConfig = ValidatorConfig
--   { validators :: [Endpoint]
--   , pact :: Endpoint
--   , gas :: Scientific
--   }
--   deriving (Generic, Show, FromJSON)

-- data Endpoint = Endpoint
--   { host :: String
--   , port :: Int
--   }
--   deriving (Generic, Show, FromJSON)

-- data Vote
--   = APPROVE
--   | DISAPPROVE
--   deriving (Generic, Show, Eq)
--   deriving Serialise via WineryVariant Vote

-- main :: IO ()
-- main = do
--   xs <- getArgs
--   case xs of
--     [filePath] -> do
--       eitherConfig :: Either String ValidatorConfig
--         <- eitherDecode <$> BSL.readFile filePath
--       case eitherConfig of
--         Left err -> print err
--         Right config -> do
--           var <- newChan :: IO (Chan ValidatorRequest)
--           connection <- connect [] localHostAddr (getPort $ pact config)

--           void $ forkIO $ do
--             req <- readChan var
--             case req of
--               FromSentinel scReq -> do
--                 proceed <- verifyBalance (gas config) (publicKey scReq)
--                 if proceed
--                   then do
--                     (failure, success) <- otherNodesPolling config scReq
--                     resp :: Either ConnectionError Vote
--                       <- call connection (key scReq)
--                     print "coming here---- debugging"
--                     let (disapprove, approve) =
--                           case resp of
--                             Left _ -> (failure + 1, success)
--                             Right _ -> (failure, success + 1)
--                     if approve > disapprove
--                       then undefined --return (Right APPROVE)
--                       else undefined --return (Right DISAPPROVE)
--                   else undefined --return (Right DISAPPROVE)
--               FromNode reqKeys -> do
--                 print "coming here -- from node"
--                 resp :: Either ConnectionError Vote
--                  <- call connection reqKeys
--                 undefined

--           putStrLn "Starting RPC servers on multiple nodes."
--           serverThreads <- liftIO $ forM (validators config) $ \con ->
--             async $ void $
--               serve userRequestHandlers var localHostAddr (getPort con) Nothing
--           threadDelay 1000
--           putStrLn "All RPC servers are running."
--           tid <- forM (validators config) $ \con -> do
--             let otherNodes = filter (\ep -> port ep /= port con) (validators config)
--             async $ mapM (connect [] localHostAddr . getPort) otherNodes
--           mapM_ wait tid
--           putStrLn "All connections are established."
--           mapM_ wait serverThreads
--       -- return (Right APPROVE)
--     _ -> error "not enough input. validator config file"

-- getPort :: Endpoint -> PortNumber 
-- getPort ep = toEnum $ port ep

-- verifyBalance :: Scientific -> String -> IO Bool
-- verifyBalance constGasFee pubKey = do
--   currentGas <-
--     liftIO $ runLevelDB "/sharddb" def (def, def {sync = True}) "ShardSpace" $
--       get (BC.pack pubKey)
--   case (readMaybe . BC.unpack) =<< currentGas of
--     Nothing -> return False
--     Just cg -> return (cg > constGasFee)

-- otherNodesPolling :: ValidatorConfig -> SCRequest -> IO (Int, Int)
-- otherNodesPolling config scReq = do
--   let currentNode =
--        case idx scReq of
--           Nothing -> undefined -- wont come here, as this is already checked in sentinel
--           Just num -> validators config !! num
--       otherNodes = filter (\ep -> port ep /= port currentNode) (validators config)
--   connection <- mapM (connect [] localHostAddr . getPort) otherNodes
--   resp :: [Either ConnectionError Vote] <- mapM (`call` key scReq) connection
--   let (left, right) = partitionEithers resp
--       disApp = length $ filter (== DISAPPROVE) right
--   return (length left + disApp, length right - disApp)

-- userRequestHandlers :: RequestHandlers (Chan ValidatorRequest)
-- userRequestHandlers =
--   [ RequestHandler $ \state req -> do
--       writeChan (connectionServerState state) (FromSentinel req)
--       return ()
--   , RequestHandler $ \state req -> do
--       writeChan (connectionServerState state) (FromNode req)
--       return ()
--   ]

-- fn :: Either ConnectionError Vote -> MVar (Either ConnectionError Vote) -> (Int, Int) -> (Int, Int)
-- fn resp result (failure, success) =
--   case resp of
--     Left _ -> (failure + 1, success)
--     Right _ -> do
--       result' <- takeMVar result
--       case result' of
--         Left _ -> (failure + 1, success)
--         Right DISAPPROVE -> (failure + 1, success)
--         Right APPROVE -> (failure, success + 1)

-- currentNodePolling :: Text -> IO (Either ConnectionError Vote)
-- currentNodePolling scReqKey = do
--   case eitherDecodeStrict' (encodeUtf8 scReqKey) :: Either String P.RequestKeys of
--     Left err -> return (Left $ ExceptionError err) -- decode failure
--     Right (P.RequestKeys reqKey) -> do
--       (dispatch, logFn, gcm, timeCache', mPubConsensus', rconf) <- validatorHelper
--       let pub = Publish mPubConsensus' dispatch timeCache' (_nodeId rconf)
--           conf' = ApiEnv logFn dispatch gcm pub
--       res <- runExceptT $ runReaderT (pollHandler (P.Poll reqKey)) conf'
--       print "coming here after poll handler............"
--       print res
--       case res of
--         Left err -> return (Left $ ExceptionError (show err))-- Server Error
--         Right (P.PollResponses (pollResponse :: HM.HashMap P.RequestKey (P.CommandResult P.Hash))) -> do
--           case HM.lookup (NE.head reqKey) pollResponse of
--             Nothing -> undefined -- no pact respone for the req key
--             Just cmdResult ->
--               case _pactResult (_crResult cmdResult) of
--                 Left err -> return (Left $ ExceptionError (show err)) -- Left PactError
--                 Right _ -> return (Right APPROVE) -- Right PactValue