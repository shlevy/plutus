{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.PAB.CliSpec
    ( tests
    ) where

import qualified Cardano.BM.Configuration.Model      as CM
import           Cardano.BM.Data.Severity            (Severity (..))
import           Cardano.BM.Data.Trace               (Trace)
import           Cardano.BM.Setup                    (setupTrace_)
import qualified Cardano.Wallet.Client               as Wallet.Client
import           Cardano.Wallet.Types                (WalletInfo (..))
-- import           ContractExample                     (ExampleContracts (..))
import           Control.Concurrent
import           Control.Concurrent.Async            (async)
import           Control.Concurrent.Availability     (available, newToken, starting)
import           Control.Monad                       (forM_, forever, void, when)
import           Data.Aeson                          (FromJSON, ToJSON, toJSON)
import           Data.Proxy                          (Proxy (Proxy))
import qualified Data.Text                           as Text
import           Data.Text.Prettyprint.Doc
import           Data.Typeable                       (Typeable)
import           Data.Yaml                           (decodeFileThrow)
import           GHC.Generics                        (Generic)
import           Ledger.Ada                          (lovelaceValueOf)
import           Network.HTTP.Client                 (defaultManagerSettings, managerModifyRequest, newManager,
                                                      setRequestIgnoreStatus)
import           Plutus.Contract
import           Plutus.Contracts.GameStateMachine   (GuessArgs (..), LockArgs (..))
import qualified Plutus.Contracts.PingPong           as PingPong
import           Plutus.PAB.App                      (StorageBackend (..))
import qualified Plutus.PAB.App                      as App
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.Config        (defaultConfig)
import qualified Plutus.PAB.Monitoring.Monitoring    as LM
import           Plutus.PAB.Monitoring.PABLogMsg     (AppMsg (..))
import           Plutus.PAB.Monitoring.Util          (PrettyObject (..), convertLog)
import           Plutus.PAB.Run.Cli                  (ConfigCommandArgs (..), runConfigCommand)
import           Plutus.PAB.Run.Command              (ConfigCommand (..), MockServerMode (..))
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
import           Plutus.PAB.Types                    (Config (..), WebserverConfig (..))
import           Plutus.PAB.Webserver.API            (NewAPI)
import           Plutus.PAB.Webserver.Types          (ContractActivationArgs (..))
import           Prettyprinter                       (Pretty)
import           Servant                             ((:<|>) (..))
import qualified Servant
import           Servant.Client                      (BaseUrl (..), ClientEnv, client, mkClientEnv, runClientM)
import           Test.Tasty                          (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Wallet.Types                        (ContractInstanceId (..))

tests :: TestTree
tests =
  testGroup "Plutus.PAB.Run.Cli"
    [ restoreContractStateTests
    ]

data TestingContracts = PingPong
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions TestingContracts where
  getDefinitions = [ PingPong ]
  getSchema _    = Builtin.endpointsToSchemas @PingPong.PingPongSchema
  getContract _  = SomeBuiltin pingPong

pingPong = do
  _ <- PingPong.initialise @()
  PingPong.runPong @()
  PingPong.runPing @()
  PingPong.runPong @()

instance HasPSTypes TestingContracts where
  psTypes _ = undefined

instance Pretty TestingContracts where
  pretty = viaShow

run = do
  -- Load the testing config
  pabConfig :: Config <- decodeFileThrow @IO "./test/full/testing-plutus-pab.yaml"
  logConfig <- defaultConfig

  CM.setMinSeverity logConfig Error

  (trace :: Trace IO (PrettyObject (AppMsg (Builtin a))), switchboard) <- setupTrace_ logConfig "pab"

  let cmd = ForkCommands
              [ MockNode WithMockServer
              , ChainIndex
              , Metadata
              , MockWallet
              , PABWebserver
              ]

  let mkArgs availability = ConfigCommandArgs
                { ccaTrace = convertLog PrettyObject trace
                , ccaLoggingConfig = logConfig
                , ccaPABConfig = pabConfig
                , ccaAvailability = availability
                , ccaStorageBackend = BeamSqliteBackend
                }

  -- Ensure the db is set up
  App.migrate (convertLog (PrettyObject . PABMsg) trace) (dbConfig pabConfig)

  -- Spin up the servers
  availability <- newToken
  a <- async $ runConfigCommand (Builtin.handleBuiltin @TestingContracts) (mkArgs availability) cmd

  -- -- Wait for them to be started
  threadDelay $ 3 * 1000000

  -- 1. Make a wallet: `/wallet/create`
  manager <- newManager defaultManagerSettings

  let newApiUrl@(BaseUrl sc host port path) = baseUrl (pabWebserverConfig pabConfig)
      walletUrl = BaseUrl sc host port "/wallet"
      walletClientEnv = mkClientEnv manager walletUrl
      apiClientEnv = mkClientEnv manager newApiUrl

  Right wi <- runClientM (Wallet.Client.createWallet) walletClientEnv

  let ca w = ContractActivationArgs
                { caID = PingPong
                , caWallet = wiWallet w
                }

  let ( activateContract
        :<|> instance'
        :<|> _
        ) = client (Proxy @(NewAPI TestingContracts Integer))

  Right ci <- runClientM (activateContract (ca wi)) apiClientEnv

  let (status' :<|> endpoint :<|> stop') = instance' . Text.pack . show . unContractInstanceId $ ci

  -- Now we have a contract instance, let's run endpoints

  let endpoints = [ "initialise", "pong", "ping" ]

  forM_ endpoints $ \e -> do
    putStrLn $ "Calling endpoint: " ++ e
    x' <- runClientM (endpoint e (toJSON ())) apiClientEnv
    putStrLn $ show x'
    threadDelay $ 10 * 1_000_000

  pure ()

restoreContractStateTests :: TestTree
restoreContractStateTests =
  testGroup "restoreContractState scenarios"
    [ testCase "Some thing" $
        undefined
    ]
