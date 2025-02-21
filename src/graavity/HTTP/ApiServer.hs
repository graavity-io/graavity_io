{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module graavity.HTTP.ApiServer
  ( ApiV1API'
  , apiV1API'
  , configChangeClient
  , listenClient
  , localClient
  , pollClient
  , privateClient
  , runApiServer
  , sendClient
  , versionClient
  , pollHandler
  , ApiEnv (..)
  ) where

import Prelude hiding (log)
import Codec.Winery
import Control.Lens
import Control.Concurrent
import Control.Exception.Lifted (catch, catches, SomeException, throw, try, throwIO)
import qualified Control.Exception.Lifted as Ex (Handler(..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import GHC.Generics
import Data.Aeson hiding (defaultOptions, Result(..))
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Serialize as SZ
import qualified Data.Set as Set
import Data.Proxy
import Data.Scientific
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Thyme.Clock
import Data.Version (showVersion)
import System.Random

-- Cabal version info
import Paths_graavity (version)

import Network.Wai.Handler.Warp hiding (setPort)
import Network.Wai.Middleware.Cors
import qualified Network.RPC.Curryer.Server as Server

import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Server hiding (route)

import System.FilePath
import System.Time.Extra (sleep)

import qualified Pact.Server.API as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.API as P
import Pact.JSON.Encode (Encode, toJsonViaEncode)

import graavity.Command (SubmitCC(..))
import graavity.Config.TMVar as KCfg
import graavity.Consensus.Publish
import qualified graavity.Types.Command as K
import graavity.Types.Base
import graavity.Types.Comms
import graavity.Types.ConfigChange (ConfigChangeException(..))
import graavity.Types.Dispatch
import graavity.Types.Entity
import qualified graavity.Types.Execution as Exec
import graavity.Types.History (History(..))
import qualified graavity.Types.History as History
import graavity.Types.Private (PrivatePlaintext(..),PrivateCiphertext(..),Labeled(..))
import graavity.Types.Spec
import graavity.Private.Service (encrypt)
import graavity.Util.Util

import qualified Network.RPC.Curryer.Client  as RPC

--- Overlappable instances
--
instance Encode a => ToJSON (P.Command a) where
  toJSON = toJsonViaEncode

instance ToJSON P.RequestKeys where
  toJSON = toJsonViaEncode


-- copied from executable/Atomizer.hs
data Log =
  GasFee GasFeeLog
  | Pact [PactLog]
  deriving (Generic, ToJSON)

data GasFeeLog =
  GasFeeLog
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
  deriving (Generic, ToJSON)

data TxnDetails =
  TxnDetails
    { txnId :: Maybe Int
    , fromKey :: String
    , fromKeyAmt :: Maybe Scientific
    , toKey :: String
    , toKeyAmt :: Scientific
    , txnAmt :: Scientific
    , txnTimestamp :: String -- todo: confirm the type
    , txnStatus :: TxnStatus
    }
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant TxnDetails

data TxnStatus =
  SUCCESS | FAILED | PENDING
  deriving (Generic, ToJSON)
  deriving Serialise via WineryVariant TxnStatus

data ServerRequest
  = FromShard Text
  | FromValidator Text
  deriving (Generic, Show)

data PactType =
  PactType
    { cmdReq :: Text -- original req
    , lOrS :: String -- "LOCAL" | "SEND"
    }
  deriving (Generic, Show, FromJSON)
  deriving Serialise via WineryVariant PactType

data PactLog = PactLog
 { result :: Text -- P.CommandResult Hash
 }
 deriving (Generic, ToJSON)
 deriving Serialise via WineryVariant PactLog

instance ToJSON (P.CommandResult Hash) where
  toJSON = toJsonViaEncode

data AtomizerConfig =
  AtomizerConfig
    { atomizer :: Endpoint
    }
  deriving (Generic, FromJSON, Show)

-- copied type from executables/Sentinels.hs
data Endpoint =
  Endpoint
    { host :: String
    , port :: Int
    }
  deriving (Generic, FromJSON, Show)
----------------------------------------------------------------------------------------------------
#if !MIN_VERSION_servant(0,16,0)
type ServerError = ServantErr
#endif

type ApiPrivate = "private"
  :> ReqBody '[JSON] (P.Command Text)
  :> Post '[JSON] P.RequestKeys

type ApiConfigChange = "configChange"
  :> ReqBody '[JSON] SubmitCC
  :> Post '[JSON] P.RequestKeys

type ApiVersion = "version"
  :> Get '[PlainText] Text

type ApiV1API' = "api" :> "v1" :>  (P.ApiSend :<|> P.ApiPoll :<|> P.ApiListen :<|> P.ApiLocal
               :<|> ApiPrivate :<|> ApiConfigChange) :<|> ApiVersion

apiV1API' :: Proxy ApiV1API'
apiV1API' = Proxy

clientM :: Proxy ClientM
clientM = Proxy

( sendClient :<|> pollClient :<|> listenClient :<|> localClient
  :<|> privateClient :<|> configChangeClient) :<|> versionClient = apiV1API' `clientIn` clientM

type Api a = ReaderT ApiEnv (ExceptT ServerError IO) a

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiDispatch :: Dispatch
  , _aiConfig :: GlobalConfigTMVar
  , _aiPublish :: Publish
  }
makeLenses ''ApiEnv

runApiServer :: Dispatch -> KCfg.GlobalConfigTMVar -> (String -> IO ())
              -> Int -> MVar PublishedConsensus -> IO UTCTime -> IO ()
runApiServer dispatch gcm logFn port' mPubConsensus' timeCache' = do
  eitherConfig :: Either String AtomizerConfig
    <- eitherDecode <$> BSL.readFile "conf/atomizer.json"
  case eitherConfig of
    Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
    Right cfg -> do
      atconnection <- RPC.connect [] (getHostAddr $ atomizer cfg) (getPort $ atomizer cfg) 
      logFn $ "[Service|API]: starting on port " ++ show port'
      rconf <- readCurrentConfig gcm
      let conf' = ApiEnv logFn dispatch gcm $
            Publish
            mPubConsensus'
            dispatch
            timeCache'
            (_nodeId rconf)
      let logFilePrefix = _logDir rconf </> show (_alias (_nodeId rconf))
          hostStaticDir' = _hostStaticDir rconf
      var <- newChan :: IO (Chan ServerRequest)

      void $ forkIO $ forever $ do
        x <- readChan var
        case x of
          FromShard req -> do
            case eitherDecodeStrict' (encodeUtf8 req) :: Either String PactType of
              Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
              Right pt ->
                case eitherDecodeStrict' (encodeUtf8 $ cmdReq pt) :: Either String (P.Command Text) of
                  Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                  Right cmd -> do
                    case lOrS pt of
                      "LOCAL" -> do
                        resp <- runExceptT $ runReaderT (localHandler cmd) conf'
                        case resp of
                          Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                          Right r -> do
                            -- send log to atomizer
                            print r
                            let cmdResult = (decodeUtf8 . BSL.toStrict) $ encode r
                            print cmdResult
                            let req = Pact [PactLog cmdResult]
                                txt = (decodeUtf8 . BSL.toStrict) $ encode req
                            atomizerRes :: Either Server.ConnectionError Text <- liftIO $ RPC.call atconnection txt
                            case atomizerRes of
                              Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                              Right r -> return ()
                      "SEND" -> do
                        resp <- runExceptT $ runReaderT (sendHandler $ P.SubmitBatch (cmd :| [])) conf'
                        case resp of
                          Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                          Right r -> do
                            -- send log to atomizer
                            print r
                            let cmdResult = (decodeUtf8 . BSL.toStrict) $ encode r
                            print cmdResult
                            let req = Pact [PactLog cmdResult]
                                txt = (decodeUtf8 . BSL.toStrict) $ encode req
                            atomizerRes :: Either Server.ConnectionError Text <- liftIO $ RPC.call atconnection txt
                            case atomizerRes of
                              Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                              Right r -> return ()
                      -- _ -> TODO:
          -- FromValidator nodeReq ->
          --   case eitherDecodeStrict' (encodeUtf8 nodeReq) :: Either String P.RequestKeys of
          --     Left err -> print err
          --     Right (P.RequestKeys reqKey) -> do
          --       res <- runExceptT $ runReaderT (pollHandler (P.Poll reqKey)) conf'
          --       print res
          --       case res of
          --         Left err -> undefined --return (Left $ Server.ExceptionError (show err))-- Server Error
          --         Right (P.PollResponses (pollResponse :: HashMap.HashMap P.RequestKey (P.CommandResult P.Hash))) -> do
          --           case HashMap.lookup (NE.head reqKey) pollResponse of
          --             Nothing -> undefined -- no pact respone for the req key
          --             Just cmdResult ->
          --               case P._pactResult (P._crResult cmdResult) of
          --                 Left err -> undefined --return (Left $ Server.ExceptionError (show err)) -- Left PactError
          --                 Right _ -> undefined --return (Right APPROVE) -- Right PactValue

      -- start a RPC server
      putStrLn "starting graavity server RPC server..."
      void $ forkIO $ void $ Server.serve userRequestHandlers var Server.allHostAddrs 8568 Nothing

      run port' $ cors (const policy) $ serve apiV1API' (apiV1Server' conf')
  where
    getHostAddr ep =
       case BS.split (toEnum $ fromEnum '.') (BC.pack $ host ep) of
          [f,s,t,fr] -> (read (BC.unpack f), read (BC.unpack s), read (BC.unpack t), read (BC.unpack fr))
          _ -> error "unexpected host"
    getPort ep = toEnum $ port ep
    policy = Just CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = ["authorization", "content-type"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Just $ 60*60*24 -- one day
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

    -- setup incoming request handlers to operate on the server's state
userRequestHandlers :: Server.RequestHandlers (Chan ServerRequest)
userRequestHandlers =
  [ Server.RequestHandler $ \state req -> do
      writeChan (Server.connectionServerState state) (FromShard req)
      return ()
  -- , Server.RequestHandler $ \state req -> do
  --     writeChan (Server.connectionServerState state) (FromValidator req)
  --     return ()
  ]

apiV1Server' :: ApiEnv -> Server ApiV1API'
apiV1Server' conf = hoistServer apiV1API' nt readerServer
  where
    nt :: forall a. Api a -> Handler a
    nt s = Handler $ runReaderT s conf
    readerServer = (sendHandler :<|> pollHandler :<|> listenHandler :<|> localHandler
                   :<|> privateHandler :<|> configChangeHandler) :<|> versionHandler

localHandler :: P.Command T.Text -> Api (P.CommandResult P.Hash)
localHandler theCmd =
  catch (do
    let P.SubmitBatch neCmds = P.SubmitBatch (theCmd :| [])
    let cmd = fmap encodeUtf8 theCmd
    mv <- liftIO newEmptyMVar
    c <- view $ aiDispatch . dispExecService
    liftIO $ writeComm c (Exec.ExecLocal cmd mv)
    liftIO $ takeMVar mv )
  (\e -> dieServerError $ "Exception caught in the handler 'sendLocal' : "
                            ++ show (e :: SomeException))

sendHandler :: P.SubmitBatch -> Api P.RequestKeys
sendHandler (P.SubmitBatch neCmds) = do
    let retryCount = 30 :: Int
    go neCmds retryCount
  where
    go :: NonEmpty (P.Command Text) -> Int -> Api P.RequestKeys
    go _ 0 = dieNotFound "timeout waiting for a valid leader"
    go cmds count = catches
        (do
            rpcs <- return $ fmap buildCmdRpc cmds
            queueRpcs rpcs
        )
        [ Ex.Handler (\ (_e :: NotReadyException) -> do
            liftIO $ sleep 1
            log $ "sendPublicBatch - Current retry-until-ready count = " ++ show count
                  ++ ", trying again..."
            go cmds (count - 1))
        , Ex.Handler (\e -> dieServerError $ "Exception caught in the handler 'sendPublicBatch': "
                                ++ show (e :: SomeException)) ]

privateHandler :: P.Command Text -> Api P.RequestKeys
privateHandler cmd =
  catch (do
    conf <- view aiConfig >>= liftIO . readCurrentConfig
    unless (_ecSending $ _entity conf)
      $ dieServerError "Node is not configured as private sending node"
    let (P.SubmitBatch cmds) = P.SubmitBatch (cmd :| [])
    log $ "privateHandler: received batch of " ++ show (length cmds)
    when (null cmds) $ dieNotFound "Empty Batch"
    rpcs <- mapM (cmdToWire conf) cmds
    queueRpcs rpcs )
  (\e -> dieServerError
    $ "Exception caught in the handler 'sendPrivateBatch': " ++ show (e :: SomeException))

configChangeHandler :: SubmitCC -> Api P.RequestKeys
configChangeHandler (SubmitCC ccCmds) =
  catch (do
    log $ "ApiServer - Cluster Change command received"
    when (null ccCmds) $ dieNotFound "Empty cluster change batch"
    when (length ccCmds /= 2) $ dieNotFound "Exactly two cluster change commands required -- one Transitional, one Final"
    let transCmd = head ccCmds
    let finalCmd = head $ tail $ ccCmds
    let transRpc = buildCCCmdRpc transCmd
    log $ "ApiServer - queuing RPC for transitional CC request"
    _ <- queueRpcs (transRpc :| []) -- NonEmpty List
    let transListenerReq = P.ListenerRequest (fst transRpc) --listen for the result of the transitional command
    ccTransResult <- listenFor transListenerReq
    case ccTransResult of
      K.ClusterChangeSuccess -> do
        log "Transitional CC was sucessful, now ok to send the Final"
        let finalRpc = buildCCCmdRpc finalCmd
        log $ "ApiServer - queuing RPC for final CC request"
        keys <- queueRpcs (finalRpc :| []) -- NonEmpty List
        let finalListenerReq = P.ListenerRequest (fst finalRpc)
        ccFinalResult <- listenFor finalListenerReq
        case ccFinalResult of
          K.ClusterChangeSuccess -> do
            log "Final CC was sucessful, Config change complete"
            return keys
          K.ClusterChangeFailure eFinal -> do
            let s = "Final cluster change failed: " ++ eFinal
            log s
            throw $ ConfigChangeException s
      K.ClusterChangeFailure eTrans  -> do
        let s = "Transitional cluster change failed: " ++ eTrans
        log s
        throw $ ConfigChangeException s
  )
  (\e -> dieServerError $ "Exception caught in the handler 'sendClusterChange': " ++ show (e :: SomeException))

queueRpcs :: NonEmpty (P.RequestKey, K.CMDWire) -> Api P.RequestKeys
queueRpcs rpcs = do
  p <- view aiPublish
  rks <- publish p dieServerError rpcs
  log $ "queueRpcs - returning reponse: " ++ show rks
  return rks

versionHandler :: Api T.Text
versionHandler = return $ toS cabalVersion

-- | Get version info from cabal
cabalVersion :: String
cabalVersion = showVersion version

dieNotFound :: String -> Api t
dieNotFound str = throwError err404 { errBody = BSL8.pack str }

dieServerError :: String -> Api t
dieServerError str = throwError err500 { errBody = BSL8.pack str }

cmdToWire :: KCfg.Config -> P.Command T.Text -> Api (P.RequestKey, K.CMDWire)
cmdToWire config cmd = do
  let cb@P.Command{..} = fmap encodeUtf8 cmd
  case eitherDecodeStrict' _cmdPayload of
    Left e -> dieNotFound $ "JSON payload decode failed: " ++ show e
    Right (P.Payload{..} :: P.Payload P.PrivateMeta T.Text) -> do
      addr@P.Address{..} <- addressFromMeta _pMeta cmd
      if _aFrom `Set.member` _aTo || _aFrom /= (_elName $ _ecLocal$ _entity config)
        then dieNotFound $ "sendPrivateBatch: invalid address in payload: " ++ show cmd
        else do
          pc@PrivateCiphertext{..} <- addressToCipher addr (toS <$> cb) config
          let hsh = P.pactHash $ _lPayload $ _pcEntity
              hc = K.Hashed pc $ P.hash $ _lPayload $ _pcEntity
          return (RequestKey hsh, K.PCWire $ SZ.encode hc)

addressToCipher :: P.Address -> P.Command BSL8.ByteString -> KCfg.Config -> Api PrivateCiphertext
addressToCipher P.Address{..} cmd config = do
  pchan <- view (aiDispatch.dispPrivateChannel)
  plainTxt <- liftIO $ encrypt pchan $
    PrivatePlaintext _aFrom (_alias (_nodeId config)) _aTo (SZ.encode cmd)
  case plainTxt of
    Left e -> dieServerError $ "sendPrivateBatch: encrypt failed: " ++ show e ++ "\n"
                  ++ show plainTxt ++ ", command: " ++ show cmd
    Right pc -> return pc

addressFromMeta :: P.PrivateMeta -> P.Command T.Text -> Api P.Address
addressFromMeta (P.PrivateMeta (Just addr)) _cmd = return addr
addressFromMeta (P.PrivateMeta Nothing) cmd =
  dieNotFound $ "sendPrivateBatch: missing address in payload: " ++ show cmd

pollHandler :: P.Poll -> Api P.PollResponses
pollHandler (P.Poll rks) =
  catch (do
    History.PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList (NE.toList rks))
    pResponses <- traverse toPactCmdResult possiblyIncompleteResults
    return $ P.PollResponses pResponses)
  (\e -> dieServerError $ "Exception caught in the handler pactPoll" ++ show (e :: SomeException))

toPactCmdResult :: K.CommandResult -> Api (P.CommandResult Hash)
toPactCmdResult (K.SmartContractResult K.PactContractResult{..}) = do
  let meta = K.PactResultMeta
             { _prMetaLogIndex = _pcrLogIndex
             , _prMetaLatMetrics = _pcrLatMetrics
             }
  let metaJ = toJSON meta
  return $ _pcrResult {P._crMetaData = Just metaJ}
toPactCmdResult _ = dieServerError $ "Expected SmartContractResult"

checkHistoryForResult :: HashSet P.RequestKey -> Api History.PossiblyIncompleteResults
checkHistoryForResult rks = do
  hChan <- view (aiDispatch.dispHistoryChannel)
  m <- liftIO $ newEmptyMVar
  liftIO $ writeComm hChan $ QueryForResults (rks,m)
  liftIO $ readMVar m

log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[Service|Api]: " ++ s)

parseSubmitBatch :: BSL.ByteString -> Api (BSL8.ByteString, P.SubmitBatch)
parseSubmitBatch b = do
  let z = eitherDecode b :: Either String P.SubmitBatch
  case eitherDecode b of
    (Right v :: Either String P.SubmitBatch) -> return (toS (toStrict b),v)
    Left e -> error ( "Left from parseSubmitBatch?: "
                    ++ "\n\t trying to decode: " ++ show b
                    ++ "\n\t decoded: " ++ show z
                    ++ "\n\terror: " ++ show e )

parseSubmitCC :: BSL.ByteString -> Api (BSL8.ByteString, SubmitCC)
parseSubmitCC b = case eitherDecode b of
    (Right v :: Either String SubmitCC) -> return (toS (toStrict b),v)
    Left e -> dieNotFound $ "Left from parseSubmitCC: " ++ e

listenFor :: P.ListenerRequest -> Api K.ClusterChangeResult
listenFor (P.ListenerRequest rk) = do
  hChan <- view (aiDispatch.dispHistoryChannel)
  m <- liftIO $ newEmptyMVar
  liftIO $ writeComm hChan $ RegisterListener (HashMap.fromList [(rk,m)])
  log $ "listenFor -- registered Listener for: " ++ show rk
  res <- liftIO $ readMVar m
  case res of
    History.GCed msg -> do
      let errStr = "Listener GCed for: " ++ show rk ++ " because " ++ msg
      return $ K.ClusterChangeFailure errStr
    History.ListenerResult cr -> do
      log $ "listenFor -- Listener Serviced for: " ++ show rk
      return $ K._concrResult cr

listenHandler :: P.ListenerRequest -> Api P.ListenResponse
listenHandler (P.ListenerRequest rk) = do
  (theEither :: Either SomeException P.ListenResponse) <- try $ do
    hChan <- view (aiDispatch.dispHistoryChannel)
    m <- liftIO $ newEmptyMVar
    liftIO $ writeComm hChan $ RegisterListener (HashMap.fromList [(rk,m)])
    log $ "Registered Listener for: " ++ show rk
    res <- liftIO $ readMVar m
    case res of
      History.GCed msg -> do
        log $ "Listener GCed for: " ++ show rk ++ " because " ++ msg
        dieNotFound msg
      History.ListenerResult cr -> do
        log $ "Listener Serviced for: " ++ show rk
        pactCr <- toPactCmdResult cr
        -- let pactCr = K._pcrResult $ K._scrPactResult cr
        return $ P.ListenResponse pactCr
  case theEither of
    Left e -> do
      let errStr = "Exception in registerListener: " ++ show e
      log errStr
      liftIO $ putStrLn errStr
      throw e
    Right y -> return y

