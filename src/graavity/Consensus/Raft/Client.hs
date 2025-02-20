{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module graavity.Consensus.Raft.Client where
import Protolude

import qualified Data.List as L
import qualified Data.Set as Set
import Options.Applicative hiding (completer)
import Numeric.Natural
import System.Console.Repline hiding (command)

import Raft
import Raft.Log
import Raft.Client

import qualified Examples.Raft.Socket.Client as RS
import qualified Examples.Raft.Socket.Common as RS

import graavity.Consensus.Raft.Store

--------------------
-- Client console --
--------------------

-- Clients interact with the nodes from a terminal:
-- Accepted operations are:
data ReplCmd
  = CmdAddNode NodeId
  | CmdGetNodes
  | CmdReadMetrics NodeId
  | CmdReadState
  | CmdRead Index
  | CmdReadInterval Index Index
  | CmdSet Var Natural
  | CmdIncr Var
  | CmdCreateUser PublicKey InitialAmount  -- New command to commit user creation
  | CmdTransferFunds PublicKey PublicKey Natural  -- New command to commit fund transfer
  | CmdValidateCU PublicKey   -- New command to validate user creation
  | CmdValidateTF PublicKey PublicKey Natural  -- New command to validate fund transfer
  | CmdHelp

replCmdParser :: Parser ReplCmd
replCmdParser = subparser $ mconcat
    [ command "addNode" $ info (CmdAddNode <$> strArgument (metavar "HOST")) $
        progDesc "Add nodeId to the set of nodeIds that the client will communicate with"
    , command "getNodes" $ info (pure CmdGetNodes) $
        progDesc "Return the node ids that the client is aware of"
    , command "readState" $ info (pure CmdReadState) $
        progDesc "Read leader state"
    , command "readMetrics" $ info (CmdReadMetrics <$> strArgument (metavar "HOST")) $
        progDesc "Read node metrics"
    , command "read" $ info (CmdRead <$> indexParser) $
        progDesc "Read specific entry"
    , command "readInterval" $ info (CmdReadInterval <$> indexParser <*> indexParser) $
        progDesc "Read entries in interval"
    , command "set" $ info (CmdSet <$> varParser <*> valueParser) $
        progDesc "Set variable to specific value"
    , command "incr" $ info (CmdIncr <$> varParser) $
        progDesc "Increment the value of a variable"
    , command "createUser" $ info (CmdCreateUser <$> publicKeyParser <*> valueParser) $
        progDesc "Create a new user"
    , command "transferFunds" $ info (CmdTransferFunds <$> publicKeyParser <*> publicKeyParser <*> valueParser) $
        progDesc "Transfer funds between users"
    , command "validateCU" $ info (CmdValidateCU <$> publicKeyParser) $
        progDesc "Validate user creation"
    , command "validateTF" $ info (CmdValidateTF <$> publicKeyParser <*> publicKeyParser <*> valueParser) $
        progDesc "Validate fund transfer"
    , command "help" $ info (pure CmdHelp) $
        progDesc "Show this help text"
    ]
  where
    indexParser :: Parser Index
    indexParser = argument auto (metavar "IDX")

    valueParser :: Parser Natural
    valueParser = argument auto (metavar "VALUE")

    varParser :: Parser Var
    varParser = strArgument (metavar "VAR")

    publicKeyParser :: Parser PublicKey
    publicKeyParser = strArgument (metavar "PUBLIC_KEY")

newtype ConsoleM a = ConsoleM
  { unConsoleM :: HaskelineT (RS.RaftSocketClientM Store StoreCmd) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

liftRSCM = ConsoleM . lift

-- | Evaluate and handle each line user inputs
handleConsoleCmd :: [Char] -> ConsoleM ()
handleConsoleCmd input = do
  nids <- liftRSCM clientGetNodes
  let parser = info (replCmdParser <**> helper) fullDesc
  let parseResult = execParserPure defaultPrefs parser (L.words input)
  case parseResult of
    Failure f -> putStrLn $ fst (renderFailure f "")
    CompletionInvoked _ -> print "optparse-applicative completion for the repl should never be invoked"
    Success cmd -> case cmd of
      CmdAddNode nid -> liftRSCM $ clientAddNode (toS nid)
      CmdGetNodes -> print =<< liftRSCM clientGetNodes
      CmdReadMetrics nid -> print =<< liftRSCM (clientQueryNodeMetrics nid)
      CmdReadState -> ifNodesAdded nids $
        handleResponse =<< liftRSCM (RS.socketClientRead ClientReadStateMachine)
      CmdRead n ->
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientRead (ClientReadEntries (ByIndex n)))
      CmdReadInterval low high ->
        ifNodesAdded nids $ do
          let byInterval = ByIndices $ IndexInterval (Just low) (Just high)
          handleResponse =<< liftRSCM (RS.socketClientRead (ClientReadEntries byInterval))
      CmdSet var val ->
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (Set var val))
      CmdIncr var ->
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (Incr var))
      -- Handle Create User command
      CmdCreateUser publicKey amount -> 
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (CreateUser publicKey amount))
      -- Handle Transfer Funds command
      CmdTransferFunds from to amount -> 
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (TransferFunds from to amount))
      -- Handle Validate Create User
      CmdValidateCU publicKey -> 
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (ValidateCU publicKey))
      -- Handle Validate Transfer Funds
      CmdValidateTF from to amount -> 
        ifNodesAdded nids $
          handleResponse =<< liftRSCM (RS.socketClientWrite (ValidateTF from to amount))
      CmdHelp -> do
        let fakeFail = parserFailure defaultPrefs parser (ShowHelpText Nothing) mempty
        putStrLn $ fst (renderFailure fakeFail "")

  where
    ifNodesAdded nids m
      | nids == Set.empty =
          putText "Please add some nodes to query first. Eg. `addNode localhost:3001`"
      | otherwise = m

    handleResponse :: Show a => Either Text a -> ConsoleM ()
    handleResponse res = do
      case res of
        Left err -> liftIO $ putText err
        Right resp -> liftIO $ putText (show resp)

addInitialNodes :: [ByteString] -> ConsoleM ()
addInitialNodes nodes =
  forM_ nodes $ \node ->
    liftRSCM $ clientAddNode (toS node)

clientRepl :: [ByteString] -> IO ()
clientRepl nodes = do
  let clientHost = "localhost"
  clientPort <- RS.getFreePort
  let clientId = ClientId $ RS.hostPortToNid (clientHost, show clientPort)
  clientRespChan <- RS.newClientRespChan
  RS.runRaftSocketClientM clientId mempty clientRespChan $ do
    evalRepl (const $ pure ">>> ")
      (unConsoleM . handleConsoleCmd) [] Nothing Nothing (Word completer) (unConsoleM (addInitialNodes nodes)) (pure Exit)

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let cmds =
        [ "addNode <host:port>"
        , "getNodes"
        , "incr <var>"
        , "set <var> <val>"
        , "createUser <publicKey> <initialAmount>"
        , "transferFunds <from> <to> <amount>"
        , "validateCU <publicKey>"
        , "validateTF <from> <to> <amount>"
        , "readState"
        , "read <idx>"
        , "readInterval <low> <high>"
        ]
  return $ filter (L.isPrefixOf n) cmds
