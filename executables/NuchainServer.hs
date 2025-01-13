{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import qualified Apps.Nuchain.Server as App

-- main :: IO ()
-- main = App.main

-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE InstanceSigs          #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE PolyKinds             #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- module Nuchain.HTTP.ApiServer
--   ( ApiV1API'
--   , apiV1API'
--   , configChangeClient
--   , listenClient
--   , localClient
--   , pollClient
--   , privateClient
--   , runApiServer
--   , sendClient
--   , versionClient
--   , pollHandler
--   , ApiEnv (..)
--   ) where

-- import Prelude hiding (log)
-- import Codec.Winery
-- import Control.Lens
-- import Control.Concurrent
-- import Control.Exception.Lifted (catch, catches, SomeException, throw, try, throwIO)
-- import qualified Control.Exception.Lifted as Ex (Handler(..))
-- import Control.Monad
-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import GHC.Generics
-- import Data.Aeson hiding (defaultOptions, Result(..))
-- import Data.ByteString.Lazy (toStrict)
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy.Char8 as BSL8
-- import Data.HashSet (HashSet)
-- import qualified Data.HashSet as HashSet
-- import qualified Data.HashMap.Strict as HashMap
-- import Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NE
-- import qualified Data.Serialize as SZ
-- import qualified Data.Set as Set
-- import Data.Proxy
-- import Data.Scientific
-- import Data.String.Conv (toS)
-- import qualified Data.Text as T
-- import Data.Text.Encoding
-- import Data.Thyme.Clock
-- import Data.Version (showVersion)
-- import System.Random

-- -- Cabal version info
-- import Paths_nuchain (version)

-- import Network.Wai.Handler.Warp hiding (setPort)
-- import Network.Wai.Middleware.Cors

-- import Servant.API
-- import Servant.Client
-- import Servant.Client.Core
-- import Servant.Server hiding (route)

-- import System.FilePath
-- import System.Time.Extra (sleep)

-- import qualified Pact.Server.API as P
-- import qualified Pact.Types.Runtime as P
-- import qualified Pact.Types.Command as P
-- import qualified Pact.Types.API as P
-- import Pact.JSON.Encode (Encode, toJsonViaEncode)

-- import Nuchain.Command (SubmitCC(..))
-- import Nuchain.Config.TMVar as KCfg
-- import Nuchain.Consensus.Publish
-- import qualified Nuchain.Types.Command as K
-- import Nuchain.Types.Base
-- import Nuchain.Types.Comms
-- import Nuchain.Types.ConfigChange (ConfigChangeException(..))
-- import Nuchain.Types.Dispatch
-- import Nuchain.Types.Entity
-- import qualified Nuchain.Types.Execution as Exec
-- import Nuchain.Types.History (History(..))
-- import qualified Nuchain.Types.History as History
-- import Nuchain.Types.Private (PrivatePlaintext(..),PrivateCiphertext(..),Labeled(..))
-- import Nuchain.Types.Spec
-- import Nuchain.Private.Service (encrypt)
-- import Nuchain.Util.Util

-- import qualified Network.RPC.Curryer.Client  as RPC


import Codec.Winery
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan)
import Control.Exception (throwIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (eitherDecode, eitherDecodeStrict', FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text,pack)
import Data.Text.Encoding (encodeUtf8)

import GHC.Generics (Generic)

import Network.HTTP.Simple (getResponseBody, setRequestMethod, setRequestHeader, setRequestBodyLBS, parseRequest_, httpLBS)
import qualified Network.RPC.Curryer.Client as RPC
import qualified Network.RPC.Curryer.Server as Server

import Servant.Server (err500, errBody)
import System.Process (readProcess)

-- instance Encode a => ToJSON (P.Command a) where
--   toJSON = toJsonViaEncode

-- instance ToJSON P.RequestKeys where
--   toJSON = toJsonViaEncode

-- instance ToJSON (P.CommandResult Hash) where
--   toJSON = toJsonViaEncode

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

data AtomizerConfig =
  AtomizerConfig
    { atomizer :: Endpoint
    }
  deriving (Generic, FromJSON, Show)

data Endpoint =
  Endpoint
    { host :: String
    , port :: Int
    }
  deriving (Generic, FromJSON, Show)

-- import Codec.Winery
-- import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan)
-- import Control.Exception (throwIO)
-- import COntrol.Monad (liftIO, void, forever)
-- -- import Control.Monad.IO.Class (liftIO)

-- import Data.Aeson (eitherDecodeStrict')
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BSL
-- import Data.Text.Encoding (encodeUtf8)
-- import qualified Network.RPC.Curryer.Client  as RPC
-- import Servant.Server (err500)

-- import Network.HTTP.Simple
-- import qualified Data.ByteString.Lazy.Char8 as LBS


main :: IO ()
main = do
  eitherConfig :: Either String AtomizerConfig
    <- eitherDecode <$> BSL.readFile "conf/atomizer.json"
  case eitherConfig of
    Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
    Right cfg -> do
      atconnection <- RPC.connect [] (getHostAddr $ atomizer cfg) (getPort $ atomizer cfg)
      var <- newChan :: IO (Chan ServerRequest)
      void $ forkIO $ forever $ do
        x <- readChan var
        case x of
          FromShard req -> do
            case eitherDecodeStrict' (encodeUtf8 req) :: Either String PactType of
              Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
              Right pt ->
                -- case eitherDecodeStrict' (encodeUtf8 $ cmdReq pt) :: Either String (P.Command Text) of
                --   Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                --   Right cmd -> do
                    case lOrS pt of
                      "LOCAL" -> do
                        resp <- return $ Right "undefined"
                        case resp of
                          Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show "err" }
                          Right r -> do
                            print r
                            -- let cmdResult = (decodeUtf8 . BSL.toStrict) $ encode r
                            -- print cmdResult
                            -- let req = Pact [PactLog cmdResult]
                            --     txt = (decodeUtf8 . BSL.toStrict) $ encode req
                            -- atomizerRes :: Either Server.ConnectionError Text <- liftIO $ RPC.call atconnection "txt"
                            -- case atomizerRes of
                            --   Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                            --   Right r -> return ()
                      "SEND" -> do
                        resp <- sendApiCall
                        case resp of
                          Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show "err" }
                          Right r -> do
                            print r
                            -- let cmdResult = (decodeUtf8 . BSL.toStrict) $ encode r
                            -- print cmdResult
                            -- let req = Pact [PactLog cmdResult]
                            --     txt = (decodeUtf8 . BSL.toStrict) $ encode req
                            -- atomizerRes :: Either Server.ConnectionError Text <- liftIO $ RPC.call atconnection "txt"
                            -- case atomizerRes of
                            --   Left err -> liftIO $ throwIO $ err500 { errBody = BSL.fromStrict $ BC.pack $ show err }
                            --   Right r -> return ()
      _ <- localApiCall
      print "--------------------------------------------------------------------------------------------------"
      _ <- sendApiCall
      putStrLn "starting nuchain server RPC server..."
      void $ Server.serve userRequestHandlers var Server.allHostAddrs 8568 Nothing
  where
    getHostAddr ep =
       case BS.split (toEnum $ fromEnum '.') (BC.pack $ host ep) of
          [f,s,t,fr] -> (read (BC.unpack f), read (BC.unpack s), read (BC.unpack t), read (BC.unpack fr))
          _ -> error "unexpected host"
    getPort ep = toEnum $ port ep

userRequestHandlers :: Server.RequestHandlers (Chan ServerRequest)
userRequestHandlers =
  [ Server.RequestHandler $ \state req -> do
      writeChan (Server.connectionServerState state) (FromShard req)
      return ()
  ]

localApiCall = do
  let url = "http://localhost:8085/api/v1/local"
  body <- readProcess "pact" ["-a", "example.yaml", "-l"] ""
  putStrLn "The request body of local Api is:"
  putStrLn body
  let request = setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"] -- Set headers
                $ setRequestBodyLBS (LBS.fromStrict $ encodeUtf8 $ pack body)                    -- Set body
                $ parseRequest_ url
  response <- httpLBS request
  putStrLn "The response body of local Api is:"
  LBS.putStrLn $ getResponseBody response
  return $ Right "undefined"

sendApiCall = do
  let url = "http://localhost:8085/api/v1/send"
  body <- readProcess "pact" ["-a", "example.yaml"] ""
  putStrLn "The request body of Send Api is:"
  putStrLn body
  let request = setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"] -- Set headers
                $ setRequestBodyLBS (LBS.pack body)                    -- Set body
                $ parseRequest_ url
  response <- httpLBS request
  putStrLn "The response body of Send Api is:"
  LBS.putStrLn $ getResponseBody response
  return $ Right "undefined"

-- pact -a example.yaml | curl -H "Content-Type: application/json" -d @- http://localhost:8085/api/v1/send