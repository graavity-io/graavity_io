module graavity.Consensus.Service
  ( runConsensusService
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad

import qualified Data.Set as Set
import Data.Thyme.Clock (UTCTime)

import qualified graavity.Config.ClusterMembership as CM
import graavity.Config.TMVar
import graavity.Consensus.Handle
import graavity.Consensus.Util
import graavity.Types.Crypto
import graavity.Event (foreverHeart)
import graavity.Types
import graavity.Types.Entity
import graavity.Types.Execution
import graavity.Messaging.Turbine
import qualified graavity.Types.Turbine as Turbine
import qualified graavity.Execution.Service as Exec
import qualified graavity.Types.Sender as Sender
import qualified graavity.Sender.Service as Sender
import qualified graavity.Types.Log as Log
import qualified graavity.Log.Types as Log
import qualified graavity.Log.Service as Log
import qualified graavity.Types.Evidence as Ev
import qualified graavity.Evidence.Service as Ev
import qualified graavity.PreProc.Service as PreProc
import qualified graavity.History.Service as History
import qualified graavity.HTTP.ApiServer as ApiServer
import graavity.Consensus.Publish
import graavity.Util.Util

launchApiService
  :: Dispatch
  -> GlobalConfigTMVar
  -> (String -> IO ())
  -> MVar PublishedConsensus
  -> IO UTCTime
  -> IO ()
launchApiService dispatch' rconf' debugFn' mPubConsensus' getCurrentTime' = do
  apiPort' <- _apiPort <$> readCurrentConfig rconf'
  linkAsyncTrack "ApiThread" (ApiServer.runApiServer dispatch' rconf' debugFn' apiPort' mPubConsensus' getCurrentTime')

launchHistoryService :: Dispatch
  -> (String -> IO ())
  -> IO UTCTime
  -> GlobalConfigTMVar
  -> IO ()
launchHistoryService dispatch' dbgPrint' getTimestamp' gCfg = do
  histEnv <- History.initHistoryEnv dispatch' dbgPrint' getTimestamp' gCfg
  linkAsyncTrack "HistoryThread" (History.runHistoryService histEnv Nothing)
  linkAsyncTrack "HistoryHB" (foreverHeart (_dispHistoryChannel dispatch') 1000000 HistoryBeat)

launchPreProcService :: Dispatch
  -> (String -> IO ())
  -> IO UTCTime
  -> GlobalConfigTMVar
  -> IO ()
launchPreProcService dispatch' dbgPrint' getTimestamp' gCfg = do
  rconf <- readCurrentConfig gCfg
  let preProcEnv = PreProc.initPreProcEnv dispatch' (_preProcThreadCount rconf) dbgPrint' getTimestamp'
                   (_preProcUsePar rconf) gCfg
  linkAsyncTrack "PreProcThread" (PreProc.runPreProcService (preProcEnv))
  linkAsyncTrack "PreProcHB" (foreverHeart (_dispProcessRequestChannel dispatch') 1000000 PreProcBeat)

launchEvidenceService :: Dispatch
  -> (String -> IO ())
  -> (Metric -> IO ())
  -> MVar Ev.PublishedEvidenceState
  -> GlobalConfigTMVar
  -> MVar ResetLeaderNoFollowersTimeout
  -> IO ()
launchEvidenceService dispatch' dbgPrint' publishMetric' mEvState rconf' mLeaderNoFollowers = do
  linkAsyncTrack "EvidenceThread" (Ev.runEvidenceService $! Ev.initEvidenceEnv dispatch' dbgPrint' rconf' mEvState mLeaderNoFollowers publishMetric')
  linkAsyncTrack "EvidenceHB" $ foreverHeart (_dispEvidence dispatch') 1000000 EvidenceBeat

launchExecutionService :: Dispatch
  -> (String -> IO ())
  -> (Metric -> IO ())
  -> KeySet
  -> NodeId
  -> IO UTCTime
  -> GlobalConfigTMVar
  -> MVar PublishedConsensus
  -> EntityConfig
  -> IO ()
launchExecutionService dispatch' dbgPrint' publishMetric' keySet'
                       nodeId' getTimestamp' gcm' pubConsensus ent = do
  rconf' <- readCurrentConfig gcm'
  execEnv <- return $! Exec.initExecutionEnv
    dispatch' dbgPrint' (_pactPersist rconf')
      (_logRules rconf') publishMetric' getTimestamp' gcm' ent
  pub <- return $! Publish pubConsensus dispatch' getTimestamp' nodeId'
  linkAsyncBoundTrack "ExecutionThread" (Exec.runExecutionService execEnv pub nodeId' keySet')
  linkAsyncTrack "ExecutionHB" $ foreverHeart (_dispExecService dispatch') 1000000 ExecutionBeat

launchLogService :: Dispatch
  -> (String -> IO ())
  -> (Metric -> IO ())
  -> GlobalConfigTMVar
  -> IO ()
launchLogService dispatch' dbgPrint' publishMetric' gCfg = do
  linkAsyncTrack "LogThread" (Log.runLogService dispatch' dbgPrint' publishMetric' gCfg)
  linkAsyncTrack "LogHB" $ (foreverHeart (_dispLogService dispatch') 1000000 Log.Heart)

launchSenderService :: Dispatch
  -> (String -> IO ())
  -> (Metric -> IO ())
  -> MVar Ev.PublishedEvidenceState
  -> MVar PublishedConsensus
  -> GlobalConfigTMVar
  -> IO ()
launchSenderService dispatch' dbgPrint' publishMetric' mEvState mPubCons rconf = do
  linkAsyncTrack "SenderThread" (Sender.runSenderService dispatch' rconf dbgPrint'
                                 publishMetric' mEvState mPubCons)
  linkAsyncTrack "SenderHB" $ foreverHeart (_dispSenderService dispatch') 1000000 Sender.SenderBeat

runConsensusService
  :: ReceiverEnv
  -> GlobalConfigTMVar
  -> ConsensusSpec
  -> ConsensusState
  -> IO UTCTime
  -> MVar PublishedConsensus -> IO ()
runConsensusService renv gcm spec rstate timeCache' mPubConsensus' = do
  rconf <- readCurrentConfig gcm
  let members = rconf ^. clusterMembers
      csize = 1 + CM.countOthers members
      qsize = CM.minQuorumOthers members
      changeToSize = CM.countTransitional members
      changeToQuorum = CM.minQuorumTransitional members
      publishMetric' = (spec ^. publishMetric)
      dispatch' = Turbine._turbineDispatch renv
      dbgPrint' = Turbine._turbineDebugPrint renv
      getTimestamp' = spec ^. getTimestamp
      keySet' = Turbine._turbineKeySet renv
      nodeId' = rconf ^. nodeId

  publishMetric' $ MetricClusterSize csize
  publishMetric' $ MetricAvailableSize csize
  publishMetric' $ MetricQuorumSize qsize
  publishMetric' $ MetricChangeToClusterSize changeToSize
  publishMetric' $ MetricChangeToQuorumSize changeToQuorum
  linkAsyncTrack "ReceiverThread" $ runMessageReceiver renv

  timerTarget' <- return $ (rstate ^. csTimerTarget)
  -- EvidenceService Environment
  mEvState <- newEmptyMVar
  mLeaderNoFollowers <- newEmptyMVar

  launchHistoryService dispatch' dbgPrint' getTimestamp' gcm
  launchPreProcService dispatch' dbgPrint' getTimestamp' gcm
  launchSenderService dispatch' dbgPrint' publishMetric' mEvState mPubConsensus' gcm
  launchExecutionService dispatch' dbgPrint' publishMetric' keySet' nodeId' getTimestamp' gcm mPubConsensus' (_entity rconf)
  launchEvidenceService dispatch' dbgPrint' publishMetric' mEvState gcm mLeaderNoFollowers
  launchLogService dispatch' dbgPrint' publishMetric' gcm
  launchApiService dispatch' gcm dbgPrint' mPubConsensus' getTimestamp'
  linkAsyncTrack "ConsensusHB" (foreverHeart (_dispConsensusEvent dispatch') 1000000 (ConsensusEvent . Heart))
  catchAndRethrow "ConsensusThread" $ runRWS_
    graavity
    (mkConsensusEnv gcm spec dispatch'
                    timerTarget' timeCache' mEvState mLeaderNoFollowers mPubConsensus')
    rstate

-- THREAD: SERVER MAIN
graavity :: Consensus ()
graavity = do
  la <- Log.hasQueryResult Log.LastApplied <$> queryLogs (Set.singleton Log.GetLastApplied)
  when (startIndex /= la) $ debug $ "Launch Sequence: disk sync replayed, Commit Index now " ++ show la
  logStaticMetrics
  resetElectionTimer
  handleEvents
