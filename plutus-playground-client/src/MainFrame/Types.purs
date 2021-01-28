module MainFrame.Types
  ( State(..)
  , View(..)
  , SimulatorState(..)
  , SimulatorView(..)
  , ChainSlot
  , Blockchain
  , WebData
  , WebCompilationResult
  , WebEvaluationResult
  , Query
  , HAction(..)
  , SimulatorAction(..)
  , WalletEvent(..)
  , DragAndDropEventType(..)
  , ChildSlots
  ) where

import Analytics (class IsEvent, defaultEvent, toEvent)
import Auth (AuthStatus)
import Chain.Types (Action(..))
import Chain.Types as Chain
import Clipboard as Clipboard
import Cursor (Cursor)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Editor.Types as Editor
import Gist (Gist)
import Gists.Types (GistAction(..))
import Halogen as H
import Halogen.Chartist as Chartist
import Halogen.Monaco as Monaco
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult)
import Network.RemoteData (RemoteData)
import Playground.Types (CompilationResult, ContractDemo, EvaluationResult, PlaygroundError, Simulation)
import Plutus.V1.Ledger.Tx (Tx)
import Prelude (class Eq, class Show, Unit, show, ($))
import Schema.Types (ActionEvent(..), SimulationAction(..))
import Servant.PureScript.Ajax (AjaxError)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen
import ValueEditor (ValueEvent(..))
import Web.HTML.Event.DragEvent (DragEvent)

newtype State
  = State
  { demoFilesMenuVisible :: Boolean
  , gistErrorPaneVisible :: Boolean
  -- Demo files.
  , contractDemos :: Array ContractDemo
  , currentDemoName :: Maybe String
  -- Gist support.
  , authStatus :: WebData AuthStatus
  , createGistResult :: WebData Gist
  , gistUrl :: Maybe String
  -- Navigation.
  , currentView :: View
  -- Editor.
  , editorState :: Editor.State
  , compilationResult :: WebCompilationResult
  , lastSuccessfulCompilationResult :: Maybe (InterpreterResult CompilationResult)
  -- Simulator.
  , simulatorState :: SimulatorState
  }

derive instance newtypeState :: Newtype State _

data View
  = Editor
  | Simulator

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (Editor :| [ Simulator ])

instance showView :: Show View where
  show Editor = "Editor"
  show Simulator = "Simulator"

newtype SimulatorState
  = SimulatorState
  { simulatorView :: SimulatorView
  , actionDrag :: Maybe Int
  , simulations :: Cursor Simulation
  , evaluationResult :: WebEvaluationResult
  , lastEvaluatedSimulation :: Maybe Simulation
  , blockchainVisualisationState :: Chain.State
  }

derive instance newtypeSimulatorState :: Newtype SimulatorState _

data SimulatorView
  = WalletsAndActions
  | Transactions

derive instance eqSimulatorView :: Eq SimulatorView

instance showSimulatorView :: Show SimulatorView where
  show WalletsAndActions = "WalletsAndActions"
  show Transactions = "Transactions"

type ChainSlot
  = Array Tx

type Blockchain
  = Array ChainSlot

type WebData
  = RemoteData AjaxError

type WebCompilationResult
  = WebData (Either InterpreterError (InterpreterResult CompilationResult))

type WebEvaluationResult
  = WebData (Either PlaygroundError EvaluationResult)

data Query a

data HAction
  = Init
  | Mounted
  -- Demo files.
  | ToggleDemoFilesMenu
  | LoadScript String
  -- Gist support.
  | CheckAuthStatus
  | GistAction GistAction
  -- Navigation.
  | ChangeView View
  -- Editor.
  | EditorAction Editor.Action
  | CompileProgram
  -- Simulator.
  | SimulatorAction SimulatorAction

-- SimulatorAction is also defined in Playground.Types as `ContractCall FormArgument`
-- (i.e. an "action" modelled in the simulation); maybe we can rethink these names.
-- There's also SimulationAction from Schema.Types. Not ideal.
data SimulatorAction
  = ChangeSimulatorView SimulatorView
  | AddSimulationSlot
  | SetSimulationSlot Int
  | RemoveSimulationSlot Int
  | ModifyWallets WalletEvent
  | ChangeSimulation SimulationAction
  | EvaluateActions
  | ActionDragAndDrop Int DragAndDropEventType DragEvent
  | HandleBalancesChartMessage Chartist.Message
  | ChainAction Chain.Action

data WalletEvent
  = AddWallet
  | RemoveWallet Int
  | ModifyBalance Int ValueEvent

data DragAndDropEventType
  = DragStart
  | DragEnd
  | DragEnter
  | DragOver
  | DragLeave
  | Drop

instance showDragAndDropEventType :: Show DragAndDropEventType where
  show DragStart = "DragStart"
  show DragEnd = "DragEnd"
  show DragEnter = "DragEnter"
  show DragOver = "DragOver"
  show DragLeave = "DragLeave"
  show Drop = "Drop"

type ChildSlots
  = ( editorSlot :: H.Slot Monaco.Query Monaco.Message Unit
    , balancesChartSlot :: H.Slot Chartist.Query Chartist.Message Unit
    )

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent HAction where
  toEvent Init = Nothing
  toEvent Mounted = Just $ defaultEvent "Mounted"
  -- Demo files.
  toEvent ToggleDemoFilesMenu = Nothing
  toEvent (LoadScript script) = Just $ (defaultEvent "LoadScript") { label = Just script }
  -- Gist support.
  toEvent CheckAuthStatus = Nothing
  -- TODO: put these instances in Gists.Types and replace them here with
  -- `toEvent (GistAction gistAction) = toEvent gistAction`
  toEvent (GistAction PublishGist) = Just $ (defaultEvent "Publish") { category = Just "Gist" }
  toEvent (GistAction (SetGistUrl _)) = Nothing
  toEvent (GistAction LoadGist) = Just $ (defaultEvent "LoadGist") { category = Just "Gist" }
  toEvent (GistAction (AjaxErrorPaneAction _)) = Nothing
  -- Navigation.
  toEvent (ChangeView view) = Just $ (defaultEvent "View") { label = Just $ show view }
  -- Editor.
  toEvent (EditorAction (Editor.HandleDropEvent _)) = Just $ defaultEvent "DropScript"
  toEvent (EditorAction action) = Just $ (defaultEvent "ConfigureEditor")
  toEvent CompileProgram = Just $ defaultEvent "CompileProgram"
  -- Simulator.
  toEvent (SimulatorAction simulatorAction) = toEvent simulatorAction

instance simulatorActionIsEvent :: IsEvent SimulatorAction where
  toEvent (ChangeSimulatorView simulatorView) = Just $ (defaultEvent "SimulatorView") { label = Just $ show simulatorView }
  toEvent (ChangeSimulation (PopulateAction _ _)) = Just $ (defaultEvent "PopulateAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddAction _))) = Just $ (defaultEvent "AddAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddWaitAction _))) = Just $ (defaultEvent "AddWaitAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (RemoveAction _))) = Just $ (defaultEvent "RemoveAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletValue _ _))) = Just $ (defaultEvent "SetPayToWalletValue") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletRecipient _ _))) = Just $ (defaultEvent "SetPayToWalletRecipient") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitTime _ _))) = Just $ (defaultEvent "SetWaitTime") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitUntilTime _ _))) = Just $ (defaultEvent "SetWaitUntilTime") { category = Just "Action" }
  toEvent AddSimulationSlot = Just $ (defaultEvent "AddSimulationSlot") { category = Just "Simulation" }
  toEvent (SetSimulationSlot _) = Just $ (defaultEvent "SetSimulationSlot") { category = Just "Simulation" }
  toEvent (RemoveSimulationSlot _) = Just $ (defaultEvent "RemoveSimulationSlot") { category = Just "Simulation" }
  toEvent (ModifyWallets AddWallet) = Just $ (defaultEvent "AddWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (RemoveWallet _)) = Just $ (defaultEvent "RemoveWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (ModifyBalance _ (SetBalance _ _ _))) = Just $ (defaultEvent "SetBalance") { category = Just "Wallet" }
  toEvent EvaluateActions = Just $ (defaultEvent "EvaluateActions") { category = Just "Action" }
  toEvent (ActionDragAndDrop _ eventType _) = Just $ (defaultEvent (show eventType)) { category = Just "Action" }
  toEvent (HandleBalancesChartMessage _) = Nothing
  toEvent (ChainAction (FocusTx (Just _))) = Just $ (defaultEvent "BlockchainFocus") { category = Just "Transaction" }
  toEvent (ChainAction (FocusTx Nothing)) = Nothing
  toEvent (ChainAction (ClipboardAction (Clipboard.CopyToClipboard _))) = Just $ (defaultEvent "ClipboardAction") { category = Just "CopyToClipboard" }