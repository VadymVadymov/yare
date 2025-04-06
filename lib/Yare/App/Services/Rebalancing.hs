{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant pure" #-}
module Yare.App.Services.Rebalancing
  ( Error (..)
  , Amount (..)
  , service
  , rebalanceDefaultAmount
  ) where

import Yare.Prelude

import Cardano.Api (inAnyShelleyBasedEra)
import Cardano.Api.Shelley
  ( BuildTx
  , ConwayEraOnwards
  , LedgerProtocolParameters (..)
  , Tx (..)
  , TxBodyContent
  , TxInMode (..)
  , constructBalancedTx
  , convert
  , defaultTxBodyContent
  , runExcept
  )
import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (Except)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (KeyValue ((.=)), object)
import Text.Pretty.Simple (pShow)
import Yare.Address (Addresses)
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter

instance ToJSON Error where
  toJSON err = object ["error" .= err]

{- | Used for specifying the amount of addresses to be balanced.
First n addresses will be taken (in the order of derivation from mnemonic).
-}
newtype Amount = Amount Integer
  deriving newtype (Eq, Show, ToJSON)

rebalanceDefaultAmount ∷ Amount
rebalanceDefaultAmount = Amount 5

service
  ∷ ∀ era state env
   . [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
  ⇒ env
  → Amount
  → IO ()
service env amount = do
  let NetworkInfo {currentEra} = look @(NetworkInfo era) env
  let submitQueue ∷ Submitter.Q = look env
  let storageManager ∷ StorageMgr IO state = look env
  setStorageMode storageManager Durable
  overDefaultStorage storageManager rebalance' \case
    Left err → do
      putTextLn "Error while rebalancing:"
      putTextLn . toStrict $ pShow err
      throwIO err
    Right tx → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      let txInMode = TxInMode (convert currentEra) tx
      Submitter.submit submitQueue txInMode
 where
  rebalance' ∷ state → (state, Either Error (Tx era))
  rebalance' s =
    case runExcept (runStateT (rebalance env amount) s) of
      Left e → (s, Left e)
      Right (r, s') → (s', Right r)

rebalance
  ∷ ∀ state env era
   . NetworkInfo era ∈ env
  ⇒ env
  → Amount
  → StateT state (Except Error) (Tx era)
rebalance env amount = do
  let
    network ∷ NetworkInfo era = look env
    era ∷ ConwayEraOnwards era = currentEra network
    epoch = epochInfo network
    protocolParams = protocolParameters network
    shelleyBasedEra = convert era

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra & _

    wrapError =
      RebalancingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

  tx ← either (throwError . wrapError) pure do
    constructBalancedTx
      shelleyBasedEra
      bodyContent
      _changeAddress
      empty {- overrideKeyWitnesses -}
      _inputsForBalancing
      (LedgerProtocolParameters protocolParams)
      epoch
      (systemStart network)
      mempty {- registered pools -}
      mempty {- delegations      -}
      mempty {- rewards          -}
      [_]
  pure tx

newtype Error = RebalancingTxError TxConstructionError
  deriving anyclass (Exception)
  deriving stock (Show)
