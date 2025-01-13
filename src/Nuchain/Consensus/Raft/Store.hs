{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nuchain.Consensus.Raft.Store where

import Protolude

import qualified Data.Map as Map
import Data.Serialize (Serialize)

import Numeric.Natural
import Raft

------------------------------
-- State Machine & Commands --
------------------------------

type PublicKey = ByteString
type InitialAmount = Natural
type Var = ByteString

data StoreCmd
  = CreateUser PublicKey InitialAmount
  | TransferFunds PublicKey PublicKey Natural
  | ValidateCU PublicKey                     -- Validation for user creation
  | ValidateTF PublicKey PublicKey Natural   -- Validation for fund transfer
  | Set Var Natural
  | Incr Var
  deriving stock (Show, Generic)
  deriving anyclass Serialize

type Store = Map Var Natural

instance RaftStateMachinePure Store StoreCmd where
  data RaftStateMachinePureError Store StoreCmd = StoreError [Char]
    deriving (Show, Generic, Serialize)

  type RaftStateMachinePureCtx Store StoreCmd = ()

  rsmTransition _ store cmd =
    case cmd of
      -- Existing commands...
      Set x n -> Right $ Map.insert x n store
      Incr x -> Right $ Map.adjust succ x store

      -- Validation for CreateUser
      ValidateCU publicKey ->
        if Map.member publicKey store
        then Left $ StoreError "User already exists" -- Negative vote
        else Right store -- Positive vote

      -- Validation for TransferFunds
      ValidateTF fromKey toKey amount ->
        case (Map.lookup fromKey store, Map.lookup toKey store) of
          (Just fromBalance, _) | fromBalance < amount -> 
            Left $ StoreError "Insufficient funds" -- Negative vote
          (Just _, Just _) -> Right store -- Positive vote
          _ -> Left $ StoreError "Sender or receiver not found" -- Negative vote

      -- Commit CreateUser (to be called if validation succeeds)
      CreateUser publicKey initialAmount ->
        if Map.member publicKey store
        then Right store -- Do nothing, user exists
        else Right $ Map.insert publicKey initialAmount store

      -- Commit TransferFunds (to be called if validation succeeds)
      TransferFunds fromKey toKey amount ->
        if Map.member fromKey store && Map.member toKey store
        then
          let fromBalance = store Map.! fromKey
              toBalance = store Map.! toKey
          in if fromBalance >= amount
            then Right $ Map.insert fromKey (fromBalance - amount) $ Map.insert toKey (toBalance + amount) store
            else Right store -- Not enough funds, do nothing
        else Right store -- Either 'from' or 'to' user does not exist, do nothing