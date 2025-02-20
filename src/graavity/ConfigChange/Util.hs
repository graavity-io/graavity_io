{-# LANGUAGE AllowAmbiguousTypes #-}

module graavity.ConfigChange.Util
  ( getMissingKeys
  ) where

import qualified Data.Map as Map

import graavity.Config.TMVar
import qualified graavity.Types.Crypto as KC
import graavity.Types.Base
import graavity.Types.Command (CCPayload(..))

getMissingKeys :: Config -> CCPayload -> IO [Alias]
getMissingKeys cfg payload = do
  let signerKeys = KC._siPubKey <$> _ccpSigners payload
  let filtered = filter f (Map.toList (_adminKeys cfg)) where
        f (_, k) = notElem k signerKeys
  return $ fmap fst filtered
