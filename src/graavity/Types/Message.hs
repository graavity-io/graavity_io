{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module graavity.Types.Message
  ( module X
  , RPC(..)
  , Topic(..)
  , Envelope(..)
  , InboundCMD(..)
  , InboundCMDChannel(..)
  , OutboundGeneral(..)
  , OutboundGeneralChannel(..)
  ) where

import Data.ByteString (ByteString)
import GHC.Generics
import Data.Typeable (Typeable)
import Control.Concurrent.Chan (Chan)
import Data.Sequence (Seq)
import Control.Concurrent.STM.TVar (TVar)

import graavity.Types.Base
import graavity.Types.Comms

import graavity.Types.Message.AE as X
import graavity.Types.Message.AER as X
import graavity.Types.Message.NewCMD as X
import graavity.Types.Message.RV as X
import graavity.Types.Message.RVR as X
import graavity.Types.Message.Signed as X

newtype Topic = Topic {_unTopic :: ByteString}
  deriving (Show, Eq)
newtype Envelope = Envelope { _unOutBoundMsg :: (Topic, ByteString) }
  deriving (Show, Eq)

data RPC = AE'   AppendEntries
         | AER'  AppendEntriesResponse
         | RV'   RequestVote
         | RVR'  RequestVoteResponse
         | NEW'  NewCmdRPC -- NB: this should never go in ERPC as an internal event, use NewCmd
  deriving (Show, Eq, Generic)

data InboundCMD =
  InboundCMD
  { _unInboundCMD :: (ReceivedAt, SignedRPC)} |
  InboundCMDFromApi
  { _unInboundCMDFromApi :: (ReceivedAt, NewCmdInternal)}
  deriving (Show, Eq, Typeable)

newtype OutboundGeneral = OutboundGeneral { _unOutboundGeneral :: [Envelope]}
  deriving (Show, Eq, Typeable)

newtype InboundCMDChannel = InboundCMDChannel (Chan InboundCMD, TVar (Seq InboundCMD))

newtype OutboundGeneralChannel = OutboundGeneralChannel (Chan OutboundGeneral)

instance Comms InboundCMD InboundCMDChannel where
  initComms = InboundCMDChannel <$> initCommsBatched
  readComm (InboundCMDChannel (_,m))  = readCommBatched m
  writeComm (InboundCMDChannel (c,_)) = writeCommBatched c
  {-# INLINE initComms #-}
  {-# INLINE readComm #-}
  {-# INLINE writeComm #-}

instance BatchedComms InboundCMD InboundCMDChannel where
  readComms (InboundCMDChannel (_,m)) cnt = readCommsBatched m cnt
  {-# INLINE readComms #-}
  writeComms (InboundCMDChannel (_,m)) xs = writeCommsBatched m xs
  {-# INLINE writeComms #-}

instance Comms OutboundGeneral OutboundGeneralChannel where
  initComms = OutboundGeneralChannel <$> initCommsNormal
  readComm (OutboundGeneralChannel c) = readCommNormal c
  writeComm (OutboundGeneralChannel c) = writeCommNormal c
  {-# INLINE initComms #-}
  {-# INLINE readComm #-}
  {-# INLINE writeComm #-}
