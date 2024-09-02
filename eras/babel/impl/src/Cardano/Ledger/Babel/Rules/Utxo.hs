{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.Ledger.Babel.Rules.Utxo (
  allegraToBabelUtxoPredFailure,
  babbageToBabelUtxoPredFailure,
  alonzoToBabelUtxoPredFailure,
  BabelUtxoPredFailure (..),
) where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra (
  AllegraUtxoPredFailure (..),
  validateOutsideValidityIntervalUTxO,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoUtxoEvent (UtxosEvent),
  AlonzoUtxoPredFailure (..),
  validateOutputTooBigUTxO,
  validateOutsideForecast,
  validateScriptsNotPaidUTxO,
  validateTooManyCollateralInputs,
  validateWrongNetworkInTxBody,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq)
import Cardano.Ledger.Alonzo.TxWits (nullRedeemers)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  validateCollateralContainsNonADA,
  validateCollateralEqBalance,
  validateOutputTooSmallUTxO,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  BabbageUtxoPredFailure (..),
 )
import Cardano.Ledger.Babbage.UTxO (getReferenceScriptsNonDistinct)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra, BabelUTXO, BabelUTXOS)
import Cardano.Ledger.Babel.Rules.Utxos (
  BabelUtxosPredFailure (..),
 )
import Cardano.Ledger.Babel.UTxO (babelProducedValue)
import Cardano.Ledger.BaseTypes (
  Globals (epochInfo, networkId, systemStart),
  Inject (..),
  Network,
  ProtVer (pvMajor),
  ShelleyBase,
  SlotNo,
  TxIx (TxIx),
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), Sized (sizedValue))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (
  CertState,
  certDStateL,
  certPStateL,
  certVStateL,
  lookupDepositDState,
  lookupDepositVState,
  psStakePoolParamsL,
 )
import Cardano.Ledger.Coin (Coin (unCoin), DeltaCoin, rationalToCoinViaCeiling, toDeltaCoin)
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody)
import Cardano.Ledger.Core (PParams (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryValue (MaryValue))
import Cardano.Ledger.Mary.UTxO (getConsumedMaryValue)
import Cardano.Ledger.Mary.Value (filterMultiAsset)
import Cardano.Ledger.Plutus (ExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.SafeHash (originalBytesSize)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  UtxoEnv (UtxoEnv),
  validateInputSetEmptyUTxO,
  validateOutputBootAddrAttrsTooBig,
  validateWrongNetwork,
  validateWrongNetworkWithdrawal,
 )
import Cardano.Ledger.TxIn (Fulfill, TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), balance, txInsFilter)
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (TRC),
  TransitionRule,
  failureOnNonEmpty,
  judgmentContext,
  liftSTS,
  trans,
  validate,
 )
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..), sequenceA_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (keysSet, mapKeys)
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.Word (Word64)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Real ((%))
import Lens.Micro ((^.))
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Validation (failureIf, failureUnless)

-- ======================================================

-- | Predicate failure for the Babel Era
data BabelUtxoPredFailure era
  = -- | Subtransition Failures
    UtxosFailure (PredicateFailure (EraRule "UTXOS" era))
  | -- | The bad transaction inputs
    BadInputsUTxO
      !(Set (TxIn (EraCrypto era)))
  | OutsideValidityIntervalUTxO
      -- | transaction's validity interval
      !ValidityInterval
      -- | current slot
      !SlotNo
  | MaxTxSizeUTxO
      -- | the actual transaction size
      !Integer
      -- | the max transaction size
      !Integer
  | InputSetEmptyUTxO
  | FeeTooSmallUTxO
      -- | the minimum fee for this transaction
      !Coin
      -- | the fee supplied in this transaction
      !Coin
  | ValueNotConservedUTxO
      -- | the Coin consumed by this transaction
      !(Value era)
      -- | the Coin produced by this transaction
      !(Value era)
  | -- | the set of addresses with incorrect network IDs
    WrongNetwork
      -- | the expected network id
      !Network
      -- | the set of addresses with incorrect network IDs
      !(Set (Addr (EraCrypto era)))
  | WrongNetworkWithdrawal
      -- | the expected network id
      !Network
      -- | the set of reward addresses with incorrect network IDs
      !(Set (RewardAccount (EraCrypto era)))
  | -- | list of supplied transaction outputs that are too small
    OutputTooSmallUTxO
      ![TxOut era]
  | -- | list of supplied bad transaction outputs
    OutputBootAddrAttrsTooBig
      ![TxOut era]
  | -- | list of supplied bad transaction output triples (actualSize,PParameterMaxValue,TxOut)
    OutputTooBigUTxO
      ![(Int, Int, TxOut era)]
  | InsufficientCollateral
      -- | balance computed
      !DeltaCoin
      -- | the required collateral for the given fee
      !Coin
  | -- | The UTxO entries which have the wrong kind of script
    ScriptsNotPaidUTxO
      !(UTxO era)
  | ExUnitsTooBigUTxO
      -- | Max EXUnits from the protocol parameters
      !ExUnits
      -- | EXUnits supplied
      !ExUnits
  | -- | The inputs marked for use as fees contain non-ADA tokens
    CollateralContainsNonADA !(Value era)
  | -- | Wrong Network ID in body
    WrongNetworkInTxBody
      -- | Actual Network ID
      !Network
      -- | Network ID in transaction body
      !Network
  | -- | slot number outside consensus forecast range
    OutsideForecast
      !SlotNo
  | -- | There are too many collateral inputs
    TooManyCollateralInputs
      -- | Max allowed collateral inputs
      !Natural
      -- | Number of collateral inputs
      !Natural
  | NoCollateralInputs
  | -- | The collateral is not equivalent to the total collateral asserted by the transaction
    IncorrectTotalCollateralField
      -- | collateral provided
      !DeltaCoin
      -- | collateral amount declared in transaction body
      !Coin
  | -- | list of supplied transaction outputs that are too small,
    -- together with the minimum value for the given output.
    BabbageOutputTooSmallUTxO
      ![(TxOut era, Coin)]
  | -- | TxIns that appear in both inputs and reference inputs
    BabbageNonDisjointRefInputs
      !(NonEmpty (TxIn (EraCrypto era)))
  | CheckRqTxFailure
  | CheckLinearFailure
  | MoreThanOneInvalidTransaction
  | CollForPrecValidFailure !Coin
  | CollForPrecInvalidFailure !Coin
  | CollInUtxoValidFailure
  | CollInUtxoInvalidFailure
  | BadFulfillsFRxO
      !(Set (Fulfill (EraCrypto era)))
  | DependsOnZoneOutput
  deriving (Generic)

type instance EraRuleFailure "UTXO" (BabelEra c) = BabelUtxoPredFailure (BabelEra c)

type instance EraRuleEvent "UTXO" (BabelEra c) = AlonzoUtxoEvent (BabelEra c)

instance InjectRuleFailure "UTXO" BabelUtxoPredFailure (BabelEra c)

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure (BabelEra c) where
  injectFailure = babbageToBabelUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure (BabelEra c) where
  injectFailure = alonzoToBabelUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure (BabelEra c) where
  injectFailure =
    allegraToBabelUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure (BabelEra c) where
  injectFailure = allegraToBabelUtxoPredFailure

instance InjectRuleFailure "UTXO" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure =
    alonzoToBabelUtxoPredFailure
      . Alonzo.UtxosFailure
      . injectFailure

deriving instance
  ( Era era
  , Show (Value era)
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , Show (TxOut era)
  , Show (Script era)
  , Show (TxIn (EraCrypto era))
  ) =>
  Show (BabelUtxoPredFailure era)

deriving instance
  ( Era era
  , Eq (Value era)
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Eq (TxOut era)
  , Eq (Script era)
  , Eq (TxIn (EraCrypto era))
  ) =>
  Eq (BabelUtxoPredFailure era)

deriving via
  InspectHeapNamed "BabelUtxoPred" (BabelUtxoPredFailure era)
  instance
    NoThunks (BabelUtxoPredFailure era)

instance
  ( Era era
  , NFData (Value era)
  , NFData (TxOut era)
  , NFData (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  NFData (BabelUtxoPredFailure era)

--------------------------------------------------------------------------------
-- BabelUTXO STS
--------------------------------------------------------------------------------

-- TODO WG MIDGROUND
-- UTXO
-- The UTXO rule is updated by :

-- removing the produced = consumed check (this is moved to the SWAPS rule)
-- changing the minfee calculation to no longer include + pp .b term
-- in feesOK, the collateral must now include + pp .b, so that
-- (coin bal * 100) ≥ᵇ (txfee * pp .collateralPercentage) + pp .b

{- CIP-0118#UTXO-rule

The only difference here is that the transaction max size check:

txsize tx ≤ maxTxSize pp
runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

...has been moved to the SWAPS rule (CIP-0118#SWAPS-rule). This is because we now
need to ensure that the size of all transactions within a zone is within the max
size of a single transaction, and, naturally, if that condition is satisfied, it
must also be implicitly satisfied for all individual transactions.

Jump to CIP-0118#UTXOS-rule to continue... -}

-- | The UTxO transition rule for the Babbage eras.
utxoTransition ::
  forall era.
  ( EraUTxO era
  , AlonzoEraTxWits era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , Environment (EraRule "UTXO" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , STS (EraRule "UTXO" era)
  , -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (EraRule "UTXO" era)
  , Environment (EraRule "UTXOS" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , InjectRuleFailure "UTXO" BabelUtxoPredFailure era
  , ConwayEraTxBody era
  ) =>
  TransitionRule (EraRule "UTXO" era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp _certState, utxos, tx) <- judgmentContext
  let utxo = utxosUtxo utxos

  {-   txb := txbody tx   -}
  let txBody = tx ^. bodyTxL
      allInputs = txBody ^. allInputsTxBodyF
      refInputs :: Set (TxIn (EraCrypto era))
      refInputs = txBody ^. referenceInputsTxBodyL
      inputs :: Set (TxIn (EraCrypto era))
      inputs = txBody ^. inputsTxBodyL

  {- inputs ∩ refInputs = ∅ -}
  runTest $ disjointRefInputs pp inputs refInputs

  {- ininterval slot (txvld txb) -}
  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ Alonzo.validateOutsideForecast ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  {-   feesOK pp tx utxo   -}
  validate $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb -}
  {- (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo   -}
  runTest $ validateBadInputsUTxO utxo allInputs

  {-   adaID ∉ supp mint tx - check not needed because mint field of type MultiAsset
   cannot contain ada -}

  {-   ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout ∗ coinsPerUTxOByte pp) -}
  let allSizedOutputs = txBody ^. allSizedOutputsTxBodyF
  runTest $ validateOutputTooSmallUTxO pp allSizedOutputs

  let allOutputs = fmap sizedValue allSizedOutputs
  {-   ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ Alonzo.validateOutputTooBigUTxO pp allOutputs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ Alonzo.validateWrongNetworkInTxBody netId txBody

  {- txsize tx ≤ maxTxSize pp -}
  -- We've moved this to the SWAPS rule. See https://github.com/IntersectMBO/formal-ledger-specifications/commit/c3e18ac1d3da92dd4894bbc32057a143f9720f52#diff-5f67369ed62c0dab01e13a73f072b664ada237d094bbea4582365264dd163bf9
  -- runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  -- Don't need this either since we're applying it to the whole zone in SWAPS
  {-   totExunits tx ≤ maxTxExUnits pp    -}
  -- runTest $ Alonzo.validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runTest $ Alonzo.validateTooManyCollateralInputs pp txBody

  trans @(EraRule "UTXOS" era) =<< coerce <$> judgmentContext

validateBadInputsUTxO ::
  UTxO era ->
  Set (TxIn (EraCrypto era)) ->
  Test (BabelUtxoPredFailure era)
validateBadInputsUTxO utxo txins =
  failureUnless (Set.null badInputs) $
    BadInputsUTxO badInputs
  where
    {- inputs ➖ dom utxo -}
    badInputs = Set.filter (`Map.notMember` unUTxO utxo) txins

-- \| Test that inputs and refInpts are disjoint, in Babel and later Eras.
disjointRefInputs ::
  forall era.
  EraPParams era =>
  PParams era ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Test (BabelUtxoPredFailure era)
disjointRefInputs pp inputs refInputs =
  when
    (pvMajor (pp ^. ppProtocolVersionL) > eraProtVerHigh @(BabbageEra (EraCrypto era)))
    (failureOnNonEmpty common BabbageNonDisjointRefInputs)
  where
    common = inputs `Set.intersection` refInputs

instance
  forall era.
  ( EraTx era
  , EraUTxO era
  , ConwayEraTxBody era
  , AlonzoEraTxWits era
  , EraRule "UTXO" era ~ BabelUTXO era
  , InjectRuleFailure "UTXO" ShelleyUtxoPredFailure era
  , InjectRuleFailure "UTXO" AllegraUtxoPredFailure era
  , InjectRuleFailure "UTXO" AlonzoUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXO" BabelUtxoPredFailure era
  , Embed (EraRule "UTXOS" era) (BabelUTXO era)
  , Environment (EraRule "UTXOS" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , PredicateFailure (EraRule "UTXO" era) ~ BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  STS (BabelUTXO era)
  where
  type State (BabelUTXO era) = UTxOState era
  type Signal (BabelUTXO era) = Tx era
  type Environment (BabelUTXO era) = Shelley.UtxoEnv era
  type BaseM (BabelUTXO era) = ShelleyBase
  type PredicateFailure (BabelUTXO era) = BabelUtxoPredFailure era
  type Event (BabelUTXO era) = AlonzoUtxoEvent era

  initialRules = []

  transitionRules = [utxoTransition @era]

instance
  ( Era era
  , STS (BabelUTXOS era)
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  , Event (EraRule "UTXOS" era) ~ Event (BabelUTXOS era)
  ) =>
  Embed (BabelUTXOS era) (BabelUTXO era)
  where
  wrapFailed = UtxosFailure
  wrapEvent = Alonzo.UtxosEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era
  , EncCBOR (TxOut era)
  , EncCBOR (Value era)
  , EncCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  EncCBOR (BabelUtxoPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxosFailure a -> Sum (UtxosFailure @era) 0 !> To a
      BadInputsUTxO ins -> Sum (BadInputsUTxO @era) 1 !> To ins
      OutsideValidityIntervalUTxO a b -> Sum OutsideValidityIntervalUTxO 2 !> To a !> To b
      MaxTxSizeUTxO a b -> Sum MaxTxSizeUTxO 3 !> To a !> To b
      InputSetEmptyUTxO -> Sum InputSetEmptyUTxO 4
      FeeTooSmallUTxO a b -> Sum FeeTooSmallUTxO 5 !> To a !> To b
      ValueNotConservedUTxO a b -> Sum (ValueNotConservedUTxO @era) 6 !> To a !> To b
      WrongNetwork right wrongs -> Sum (WrongNetwork @era) 7 !> To right !> To wrongs
      WrongNetworkWithdrawal right wrongs -> Sum (WrongNetworkWithdrawal @era) 8 !> To right !> To wrongs
      OutputTooSmallUTxO outs -> Sum (OutputTooSmallUTxO @era) 9 !> To outs
      OutputBootAddrAttrsTooBig outs -> Sum (OutputBootAddrAttrsTooBig @era) 10 !> To outs
      OutputTooBigUTxO outs -> Sum (OutputTooBigUTxO @era) 11 !> To outs
      InsufficientCollateral a b -> Sum InsufficientCollateral 12 !> To a !> To b
      ScriptsNotPaidUTxO a -> Sum ScriptsNotPaidUTxO 13 !> To a
      ExUnitsTooBigUTxO a b -> Sum ExUnitsTooBigUTxO 14 !> To a !> To b
      CollateralContainsNonADA a -> Sum CollateralContainsNonADA 15 !> To a
      WrongNetworkInTxBody a b -> Sum WrongNetworkInTxBody 16 !> To a !> To b
      OutsideForecast a -> Sum OutsideForecast 17 !> To a
      TooManyCollateralInputs a b -> Sum TooManyCollateralInputs 18 !> To a !> To b
      NoCollateralInputs -> Sum NoCollateralInputs 19
      IncorrectTotalCollateralField c1 c2 -> Sum IncorrectTotalCollateralField 20 !> To c1 !> To c2
      BabbageOutputTooSmallUTxO x -> Sum BabbageOutputTooSmallUTxO 21 !> To x
      BabbageNonDisjointRefInputs x -> Sum BabbageNonDisjointRefInputs 22 !> To x
      CheckRqTxFailure -> Sum CheckRqTxFailure 23
      CheckLinearFailure -> Sum CheckLinearFailure 24
      MoreThanOneInvalidTransaction -> Sum MoreThanOneInvalidTransaction 25
      CollForPrecValidFailure c -> Sum CollForPrecValidFailure 26 !> To c
      CollForPrecInvalidFailure c -> Sum CollForPrecInvalidFailure 27 !> To c
      CollInUtxoValidFailure -> Sum CollInUtxoValidFailure 28
      CollInUtxoInvalidFailure -> Sum CollInUtxoInvalidFailure 29
      BadFulfillsFRxO fs -> Sum (BadFulfillsFRxO @era) 30 !> To fs
      DependsOnZoneOutput -> Sum DependsOnZoneOutput 31

instance
  ( Era era
  , DecCBOR (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  DecCBOR (BabelUtxoPredFailure era)
  where
  decCBOR = decode . Summands "BabelUtxoPred" $ \case
    0 -> SumD UtxosFailure <! From
    1 -> SumD BadInputsUTxO <! From
    2 -> SumD OutsideValidityIntervalUTxO <! From <! From
    3 -> SumD MaxTxSizeUTxO <! From <! From
    4 -> SumD InputSetEmptyUTxO
    5 -> SumD FeeTooSmallUTxO <! From <! From
    6 -> SumD ValueNotConservedUTxO <! From <! From
    7 -> SumD WrongNetwork <! From <! From
    8 -> SumD WrongNetworkWithdrawal <! From <! From
    9 -> SumD OutputTooSmallUTxO <! From
    10 -> SumD OutputBootAddrAttrsTooBig <! From
    11 -> SumD OutputTooBigUTxO <! From
    12 -> SumD InsufficientCollateral <! From <! From
    13 -> SumD ScriptsNotPaidUTxO <! D (UTxO <$> decCBOR)
    14 -> SumD ExUnitsTooBigUTxO <! From <! From
    15 -> SumD CollateralContainsNonADA <! From
    16 -> SumD WrongNetworkInTxBody <! From <! From
    17 -> SumD OutsideForecast <! From
    18 -> SumD TooManyCollateralInputs <! From <! From
    19 -> SumD NoCollateralInputs
    20 -> SumD IncorrectTotalCollateralField <! From <! From
    21 -> SumD BabbageOutputTooSmallUTxO <! From
    22 -> SumD BabbageNonDisjointRefInputs <! From
    23 -> SumD CheckRqTxFailure
    24 -> SumD CheckLinearFailure
    25 -> SumD MoreThanOneInvalidTransaction
    26 -> SumD CollForPrecValidFailure <! From
    27 -> SumD CollForPrecInvalidFailure <! From
    28 -> SumD CollInUtxoValidFailure
    29 -> SumD CollInUtxoInvalidFailure
    30 -> SumD BadFulfillsFRxO <! From
    31 -> SumD DependsOnZoneOutput
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToBabelUtxoPredFailure ::
  forall era.
  BabbageUtxoPredFailure era ->
  BabelUtxoPredFailure era
babbageToBabelUtxoPredFailure = \case
  Babbage.AlonzoInBabbageUtxoPredFailure a -> alonzoToBabelUtxoPredFailure a
  Babbage.IncorrectTotalCollateralField c1 c2 -> IncorrectTotalCollateralField c1 c2
  Babbage.BabbageOutputTooSmallUTxO ts -> BabbageOutputTooSmallUTxO ts
  Babbage.BabbageNonDisjointRefInputs ts -> BabbageNonDisjointRefInputs ts

alonzoToBabelUtxoPredFailure ::
  forall era.
  AlonzoUtxoPredFailure era ->
  BabelUtxoPredFailure era
alonzoToBabelUtxoPredFailure = \case
  Alonzo.BadInputsUTxO x -> BadInputsUTxO x
  Alonzo.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Alonzo.MaxTxSizeUTxO x y -> MaxTxSizeUTxO x y
  Alonzo.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Alonzo.FeeTooSmallUTxO c1 c2 -> FeeTooSmallUTxO c1 c2
  Alonzo.ValueNotConservedUTxO vc vp -> ValueNotConservedUTxO vc vp
  Alonzo.WrongNetwork x y -> WrongNetwork x y
  Alonzo.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Alonzo.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Alonzo.UtxosFailure x -> UtxosFailure x
  Alonzo.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Alonzo.TriesToForgeADA ->
    error
      "Impossible case, soon to be removed. See: https://github.com/IntersectMBO/cardano-ledger/issues/4085"
  Alonzo.OutputTooBigUTxO xs ->
    let
      -- TODO: Remove this once the other eras will make the switch from Integer to Int
      -- as per #4015.
      -- https://github.com/IntersectMBO/cardano-ledger/issues/4085
      toRestricted :: (Integer, Integer, TxOut era) -> (Int, Int, TxOut era)
      toRestricted (sz, mv, out) = (fromIntegral sz, fromIntegral mv, out)
     in
      OutputTooBigUTxO $ map toRestricted xs
  Alonzo.InsufficientCollateral c1 c2 -> InsufficientCollateral c1 c2
  Alonzo.ScriptsNotPaidUTxO u -> ScriptsNotPaidUTxO u
  Alonzo.ExUnitsTooBigUTxO e1 e2 -> ExUnitsTooBigUTxO e1 e2
  Alonzo.CollateralContainsNonADA v -> CollateralContainsNonADA v
  Alonzo.WrongNetworkInTxBody nid nidb -> WrongNetworkInTxBody nid nidb
  Alonzo.OutsideForecast sno -> OutsideForecast sno
  Alonzo.TooManyCollateralInputs n1 n2 -> TooManyCollateralInputs n1 n2
  Alonzo.NoCollateralInputs -> NoCollateralInputs

allegraToBabelUtxoPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Allegra.AllegraUtxoPredFailure era ->
  BabelUtxoPredFailure era
allegraToBabelUtxoPredFailure = \case
  Allegra.BadInputsUTxO x -> BadInputsUTxO x
  Allegra.OutsideValidityIntervalUTxO vi slotNo -> OutsideValidityIntervalUTxO vi slotNo
  Allegra.MaxTxSizeUTxO x y -> MaxTxSizeUTxO x y
  Allegra.InputSetEmptyUTxO -> InputSetEmptyUTxO
  Allegra.FeeTooSmallUTxO c1 c2 -> FeeTooSmallUTxO c1 c2
  Allegra.ValueNotConservedUTxO vc vp -> ValueNotConservedUTxO vc vp
  Allegra.WrongNetwork x y -> WrongNetwork x y
  Allegra.WrongNetworkWithdrawal x y -> WrongNetworkWithdrawal x y
  Allegra.OutputTooSmallUTxO x -> OutputTooSmallUTxO x
  Allegra.UpdateFailure x -> absurdEraRule @"PPUP" @era x
  Allegra.OutputBootAddrAttrsTooBig xs -> OutputBootAddrAttrsTooBig xs
  Allegra.TriesToForgeADA ->
    error
      "Impossible case, soon to be removed. See: https://github.com/IntersectMBO/cardano-ledger/issues/4085"
  Allegra.OutputTooBigUTxO xs -> OutputTooBigUTxO (map (0,0,) xs)

-- To Babel Fees implementers: These functions are NOT in the right place.
-- Given more time, I'd do something with the EraUTxO class.
feesOK ::
  forall era rule.
  ( EraUTxO era
  , AlonzoEraTxWits era
  , InjectRuleFailure rule AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  , InjectRuleFailure rule BabelUtxoPredFailure era
  , ConwayEraTxBody era
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Test (EraRuleFailure rule era)
feesOK pp tx u@(UTxO utxo) =
  let txBody = tx ^. bodyTxL
      collateral' = txBody ^. collateralInputsTxBodyL -- Inputs allocated to pay txfee
      -- restrict Utxo to those inputs we use to pay fees.
      utxoCollateral = eval (collateral' ◁ utxo)
      theFee = txBody ^. feeTxBodyL -- Coin supplied to pay fees
      minFee = getMinFeeTxUtxo pp tx u
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txBody
          failureUnless (minFee <= theFee) (injectFailure $ FeeTooSmallUTxO minFee theFee)
        , -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers $ tx ^. witsTxL . rdmrsTxWitsL) $
            validateTotalCollateral pp txBody utxoCollateral
        ]

validateTotalCollateral ::
  forall era rule.
  ( BabbageEraTxBody era
  , InjectRuleFailure rule AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  ) =>
  PParams era ->
  TxBody era ->
  Map.Map (TxIn (EraCrypto era)) (TxOut era) ->
  Test (EraRuleFailure rule era)
validateTotalCollateral pp txBody utxoCollateral =
  sequenceA_
    [ -- Part 3: (∀(a,_,_) ∈ range (collateral txb ◁ utxo), a ∈ Addrvkey)
      fromAlonzoValidation $ Alonzo.validateScriptsNotPaidUTxO utxoCollateral
    , -- Part 4: isAdaOnly balance
      fromAlonzoValidation $ validateCollateralContainsNonADA txBody utxoCollateral
    , -- Part 5: balance ≥ ⌈txfee txb ∗ (collateralPercent pp) / 100⌉
      fromAlonzoValidation $ validateInsufficientCollateral pp txBody bal
    , -- Part 6: (txcoll tx ≠ ◇) ⇒ balance = txcoll tx
      first (fmap injectFailure) $ validateCollateralEqBalance bal (txBody ^. totalCollateralTxBodyL)
    , -- Part 7: collInputs tx ≠ ∅
      fromAlonzoValidation $ failureIf (null utxoCollateral) (Alonzo.NoCollateralInputs @era)
    ]
  where
    bal = collAdaBalance txBody utxoCollateral
    fromAlonzoValidation = first (fmap (injectFailure @rule))

-- (coin bal * 100) ≥ᵇ (txfee * pp .collateralPercentage) + pp .b
validateInsufficientCollateral ::
  ( EraTxBody era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  TxBody era ->
  DeltaCoin ->
  Test (AlonzoUtxoPredFailure era)
validateInsufficientCollateral pp txBody bal =
  failureUnless
    (Val.scale (100 :: Int) bal >= toDeltaCoin (Val.scale collPerc txfee <+> pp ^. ppMinFeeBL))
    $ Alonzo.InsufficientCollateral bal
    $ rationalToCoinViaCeiling
    $ ((fromIntegral collPerc * unCoin txfee) + unCoin (pp ^. ppMinFeeBL)) % 100
  where
    txfee = txBody ^. feeTxBodyL -- Coin supplied to pay fees
    collPerc = pp ^. ppCollateralPercentageL

babelProducedValueFrxo ::
  (ConwayEraTxBody era, Value era ~ MaryValue (EraCrypto era)) =>
  PParams era ->
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
babelProducedValueFrxo pp isStakePool txBody =
  babelProducedValue pp isStakePool txBody