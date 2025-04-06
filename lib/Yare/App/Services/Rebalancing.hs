module Yare.App.Services.Rebalancing
  ( Error (..)
  , Amount (..)
  , service
  , rebalanceDefaultAmount
  ) where

import Yare.Prelude

import Cardano.Api (inAnyShelleyBasedEra)
import Cardano.Api.Shelley
  ( AddressInEra
  , BuildTx
  , ConwayEraOnwards
  , LedgerProtocolParameters (..)
  , Lovelace
  , ShelleyBasedEra
  , Tx (..)
  , TxBodyContent
  , TxInMode (..)
  , UTxO
  , constructBalancedTx
  , convert
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , runExcept
  , selectLovelace
  )
import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (Except)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.Map.Strict qualified as Map
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey, Addresses (externalAddresses))
import Yare.Address qualified as Address
import Yare.Address.Derivation (ledgerAddress)
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction (mkCardanoApiUtxo, witnessUtxoEntry)
import Yare.Utxo (Utxo, spendableEntries)
import Yare.Utxo qualified as Utxo

instance ToJSON Error where
  toJSON err = object ["error" .= err]

{- | Used for specifying the amount of addresses to be balanced.
First n addresses will be taken (in the order of derivation from mnemonic).
-}
newtype Amount = Amount Int
  deriving newtype (Eq, Show, ToJSON)

rebalanceDefaultAmount ∷ Amount
rebalanceDefaultAmount = Amount 5

service
  ∷ ∀ era state env
   . ( [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     , '[Utxo] ∈∈ state
     )
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
   . ( '[NetworkInfo era, Addresses] ∈∈ env
     , '[Utxo] ∈∈ state
     )
  ⇒ env
  → Amount
  → StateT state (Except Error) (Tx era)
rebalance env (Amount amount) = do
  let
    addresses = look @Addresses env
    network ∷ NetworkInfo era = look env
    era ∷ ConwayEraOnwards era = currentEra network
    epoch = epochInfo network
    protocolParams = protocolParameters network
    shelleyBasedEra ∷ ShelleyBasedEra era = convert era
    allAddresses = toList (externalAddresses (look @Addresses env))
    rebalancingAddresses ∷ [AddressWithKey] =
      take (min amount (length allAddresses)) allAddresses

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra & _

    changeAddress ∷ AddressInEra era =
      fromShelleyAddrIsSbe shelleyBasedEra . ledgerAddress $
        Address.useForChange addresses

  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputFee addresses (0 ∷ Lovelace))
      >>= maybe (throwError (RebalancingTxError NoFeeInputs)) pure

  utxoEntryForCollateral ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputCollateral addresses (0 ∷ Lovelace))
      >>= maybe (throwError (RebalancingTxError NoCollateralInputs)) pure

  totalLovelaceBalance ∷ Lovelace ←
    usingMonadState (calculateTotalBalance rebalancingAddresses)
      >>= maybe (throwError (RebalancingTxError NoCollateralInputs)) pure

  let
    inputsForBalancing ∷ UTxO era =
      mkCardanoApiUtxo era [utxoEntryForFee, utxoEntryForCollateral] <> _

    wrapError =
      RebalancingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

  either (throwError . wrapError) pure do
    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddress
      empty {- overrideKeyWitnesses -}
      inputsForBalancing
      (LedgerProtocolParameters protocolParams)
      epoch
      (systemStart network)
      mempty {- registered pools -}
      mempty {- delegations      -}
      mempty {- rewards          -}
      [ witnessUtxoEntry utxoEntryForFee
      , witnessUtxoEntry utxoEntryForCollateral
      ]

calculateTotalBalance ∷ [AddressWithKey] → Utxo → Maybe (Utxo, Lovelace)
calculateTotalBalance addresses utxo = Just (utxo, totalBalance)
 where
  totalBalance = selectLovelace $ Map.foldr' f mempty (spendableEntries utxo)
  f (addr, value) acc
    | addr `elem` (ledgerAddress <$> addresses) = acc <> value
    | otherwise = acc

newtype Error = RebalancingTxError TxConstructionError
  deriving anyclass (Exception)
  deriving stock (Show)
