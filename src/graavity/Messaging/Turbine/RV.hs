module graavity.Messaging.Turbine.RV
  ( rvAndRvrTurbine
  ) where

-- TODO: we use toList to change from a Seq to a list for `parList`, change this


import Control.Lens
import Control.Monad
import Control.Monad.Reader

import graavity.Message (signedRPCtoRPC)
import graavity.Types.Event (Event(..), ConsensusEvent(..))
import graavity.Types.Message.Signed (SignedRPC(..), Digest(..))
import graavity.Types.Comms (InboundRVorRVR(..), Comms(..))
import graavity.Types.Dispatch (dispInboundRVorRVR, dispConsensusEvent)
import graavity.Types.Turbine (ReceiverEnv(..), turbineDebugPrint, turbineKeySet, turbineDebugPrint, turbineDispatch)
import graavity.Messaging.Turbine.Util

rvAndRvrTurbine :: ReaderT ReceiverEnv IO ()
rvAndRvrTurbine = do
  getRvAndRVRs' <- view (turbineDispatch . dispInboundRVorRVR)
  enqueueEvent <- view (turbineDispatch . dispConsensusEvent)
  debug <- view turbineDebugPrint
  ks <- view turbineKeySet
  liftIO $ forever $ do
    (ts, msg) <- _unInboundRVorRVR <$> readComm getRvAndRVRs'
    case signedRPCtoRPC (Just ts) ks msg of
      Left err -> debug err
      Right v -> do
        debug $ turbineRv ++ "received " ++ show (_digType $ _sigDigest msg)
        writeComm enqueueEvent $ ConsensusEvent $ ERPC v
