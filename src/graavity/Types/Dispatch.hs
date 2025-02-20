{-# LANGUAGE TemplateHaskell #-}

module graavity.Types.Dispatch
  ( Dispatch(..)
  , initDispatch
  , dispInboundAER
  , dispInboundCMD
  , dispInboundRVorRVR
  , dispInboundGeneral
  , dispOutboundGeneral
  , dispConsensusEvent
  , dispSenderService
  , dispLogService
  , dispEvidence
  , dispExecService
  , dispHistoryChannel
  , dispProcessRequestChannel
  , dispPrivateChannel
  ) where

import Control.Lens

import Data.Typeable

import graavity.Types.Comms
import graavity.Types.Sender (SenderServiceChannel)
import graavity.Log.Types (LogServiceChannel)
import graavity.Evidence.Spec (EvidenceChannel)
import graavity.Types.Execution (ExecutionChannel)
import graavity.Types.History (HistoryChannel)
import graavity.Types.PreProc (ProcessRequestChannel)
import graavity.Types.Private (PrivateChannel)
import graavity.Types.Message (InboundCMDChannel,OutboundGeneralChannel)
import graavity.Types.Event (ConsensusEventChannel)

data Dispatch = Dispatch
  { _dispInboundAER      :: InboundAERChannel
  , _dispInboundCMD      :: InboundCMDChannel
  , _dispInboundRVorRVR  :: InboundRVorRVRChannel
  , _dispInboundGeneral  :: InboundGeneralChannel
  , _dispOutboundGeneral :: OutboundGeneralChannel
  , _dispConsensusEvent   :: ConsensusEventChannel
  , _dispSenderService   :: SenderServiceChannel
  , _dispLogService   :: LogServiceChannel
  , _dispEvidence   :: EvidenceChannel
  , _dispExecService :: ExecutionChannel
  , _dispHistoryChannel :: HistoryChannel
  , _dispProcessRequestChannel :: ProcessRequestChannel
  , _dispPrivateChannel :: PrivateChannel
  } deriving (Typeable)

initDispatch :: IO Dispatch
initDispatch = Dispatch
  <$> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms
  <*> initComms

makeLenses ''Dispatch
