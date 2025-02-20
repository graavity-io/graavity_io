{-# LANGUAGE TemplateHaskell #-}

module graavity.Types.Turbine
  ( ReceiverEnv(..)
  , turbineDispatch
  , turbineKeySet
  , turbineDebugPrint
  , restartTurbo
  ) where

import Control.Concurrent (MVar)
import Control.Lens

import graavity.Types.Crypto (KeySet(..))
import graavity.Types.Dispatch (Dispatch(..))

data ReceiverEnv = ReceiverEnv
  { _turbineDispatch :: Dispatch
  , _turbineKeySet :: KeySet
  , _turbineDebugPrint :: String -> IO ()
  , _restartTurbo :: MVar String
  }
makeLenses ''ReceiverEnv
