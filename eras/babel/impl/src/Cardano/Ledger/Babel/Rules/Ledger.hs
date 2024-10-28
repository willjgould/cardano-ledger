{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Ledger.Babel.Rules.Ledger where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx, IsValid (IsValid), totExUnits)
import Cardano.Ledger.Babel.Core (
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  InjectRuleFailure (..),
  collateralInputsTxBodyL,
  isValidTxL,
 )
import Cardano.Ledger.Babel.Era (BabelEra, BabelLEDGER, BabelSWAPS)
import Cardano.Ledger.Babel.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  SlotNo,
  epochInfo,
  strictMaybeToMaybe,
  systemStart,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
 )
import Cardano.Ledger.Core (
  EraRuleEvent,
  EraRuleFailure,
  txIdTx,
 )
import Cardano.Ledger.Shelley.API (
  LedgerState (LedgerState),
  TxIn (TxIn),
  UTxO (..),
  UTxOState (..),
 )
import Cardano.Ledger.TxIn (TxId)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  tellEvent,
  trans,
  whenFailureFree,
 )
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', folded, mapped, to, (%~), (.~), (^.), (^..), _Just)

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash, hash)
import Cardano.Ledger.Allegra.Core (EraTxBody (..))
import Cardano.Ledger.Alonzo.Core (
  AlonzoEraTxWits,
  EraSegWits (..),
  EraTx (witsTxL),
  ppCollateralPercentageL,
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext, evalPlutusScripts)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
  TagMismatchDescription (PassedUnexpectedly),
  invalidBegin,
  invalidEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), nullRedeemers)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  validateTotalCollateral,
 )
import Cardano.Ledger.Babel.Core (AlonzoEraTxBody, Value, ppMaxTxExUnitsL)
import Cardano.Ledger.Babel.Rules.Swaps (
  BabelSwapsEnv (..),
  BabelSwapsEvent,
  BabelSwapsPredFailure,
 )
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure (..))
import Cardano.Ledger.Babel.Rules.Utxos (
  BabelUtxoEnv,
  BabelUtxosPredFailure (CollectErrors, ValidationTagMismatch),
  BatchData (..),
 )
import Cardano.Ledger.Babel.Rules.Utxow (BabelUTXOW, BabelUtxowPredFailure)
import Cardano.Ledger.Babel.Tx (BabelEraTx (..), mkBabelEraTx)
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody (..))
import Cardano.Ledger.Binary (EncCBOR (..), Sized (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Binary.Decoding (DecCBOR (..))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Conway.Core (ConwayEraScript, EraGov (GovState))
import Cardano.Ledger.Conway.Governance (ConwayEraGov, ConwayGovState, GovProcedures, Proposals)
import Cardano.Ledger.Conway.Rules (CertsEnv, GovEnv)
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody)
import Cardano.Ledger.Core (
  EraIndependentTxBody,
  EraScript (..),
  EraTxOut (..),
  EraTxWits (..),
  PParams,
  TxCert,
  ppMaxTxSizeL,
  sizeTxF,
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus (
  PlutusWithContext,
  ScriptFailure (scriptFailurePlutus),
  ScriptResult (..),
 )
import Cardano.Ledger.Plutus.ExUnits (pointWiseExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.API (LedgerEnv (..))
import Cardano.Ledger.Shelley.LedgerState (AccountState, updateStakeDistribution, utxosUtxoL)
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  validateMaxTxSizeUTxO,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.UTxO (consumed, produced)
import Cardano.Ledger.TxIn (TxIx)
import Cardano.Ledger.UTxO (EraUTxO (ScriptsNeeded), balance, getMinFeeTxUtxo, txInsFilter)
import Cardano.Ledger.Val (Val ((<+>)), coin)
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless, when)
import Control.Monad.RWS (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition (validate)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (foldl'), sequenceA_, toList)
import Data.Function ((&))
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map (member)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq (..))
import Debug.Trace (trace, traceEvent)
import NoThunks.Class (NoThunks)
import Validation (failure, failureUnless)
import Validation.Combinators (whenFailure, whenFailure_)

newtype BabelLedgerPredFailure era
  = SwapsFailure (PredicateFailure (BabelSWAPS era)) -- Subtransition Failures
  deriving (Generic)

data BabelLedgerEvent era
  = SwapsEvent (Event (EraRule "SWAPS" era))
  | ZoneFailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | ZoneSuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "LEDGER" (BabelEra c) = BabelLedgerPredFailure (BabelEra c)

instance InjectRuleFailure "LEDGER" BabelLedgerPredFailure (BabelEra c)

type instance EraRuleEvent "LEDGER" (BabelEra c) = BabelLedgerEvent (BabelEra c)

instance InjectRuleFailure "LEDGER" BabelSwapsPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure (BabelEra c) where
  injectFailure = SwapsFailure . injectFailure

deriving instance
  ( Era era
  , ConwayEraScript era
  , Show (PredicateFailure (EraRule "SWAPS" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "UTXO" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  ) =>
  Show (BabelLedgerPredFailure era)

deriving instance
  ( Era era
  , ConwayEraScript era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  ) =>
  Eq (BabelLedgerPredFailure era)

deriving anyclass instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  ) =>
  NoThunks (BabelLedgerPredFailure era)

instance
  ( Era era
  , ConwayEraScript era
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  , NFData (PredicateFailure (EraRule "LEDGER" era))
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (TxCert era)
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (BabelLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  , EncCBOR (PredicateFailure (EraRule "SWAPS" era))
  , ConwayEraScript era
  ) =>
  EncCBOR (BabelLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      SwapsFailure x -> Sum (SwapsFailure @era) 1 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SWAPS" era))
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  , ConwayEraScript era
  ) =>
  DecCBOR (BabelLedgerPredFailure era)
  where
  decCBOR =
    decode $ Summands "BabelLedgerPredFailure" $ \case
      1 -> SumD SwapsFailure <! From
      n -> Invalid n

deriving instance
  ( Era era
  , ConwayEraScript era
  , Show (Event (EraRule "SWAPS" era))
  ) =>
  Show (BabelLedgerEvent era)

deriving instance
  ( Era era
  , ConwayEraScript era
  , Eq (Event (EraRule "SWAPS" era))
  ) =>
  Eq (BabelLedgerEvent era)

deriving anyclass instance
  ( Era era
  , NoThunks (Event (EraRule "SWAPS" era))
  , NoThunks (PlutusWithContext (EraCrypto era))
  ) =>
  NoThunks (BabelLedgerEvent era)

instance
  ( Era era
  , ConwayEraScript era
  , NFData (Event (EraRule "SWAPS" era))
  , NFData (TxCert era)
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (BabelLedgerEvent era)

instance
  ( Era era
  , EncCBOR (Event (EraRule "SWAPS" era))
  , EncCBOR (Event (EraRule "LEDGER" era))
  , EncCBOR (PlutusWithContext (EraCrypto era))
  ) =>
  EncCBOR (BabelLedgerEvent era)
  where
  encCBOR =
    encode . \case
      SwapsEvent x -> Sum (SwapsEvent @era) 1 !> To x
      ZoneFailedPlutusScriptsEvent x -> Sum (ZoneFailedPlutusScriptsEvent @era) 2 !> To x
      ZoneSuccessfulPlutusScriptsEvent x -> Sum (ZoneSuccessfulPlutusScriptsEvent @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (Event (EraRule "SWAPS" era))
  , DecCBOR (PlutusWithContext (EraCrypto era))
  ) =>
  DecCBOR (BabelLedgerEvent era)
  where
  decCBOR =
    decode $ Summands "BabelLedgerEvent" $ \case
      1 -> SumD ZoneFailedPlutusScriptsEvent <! From
      2 -> SumD ZoneSuccessfulPlutusScriptsEvent <! From
      n -> Invalid n
instance
  ( EraRule "LEDGER" era ~ BabelLEDGER era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "UTXO" era))
  , Show (PredicateFailure (EraRule "UTXO" era))
  , ConwayEraPParams era
  , Environment (EraRule "SWAPS" era) ~ BabelSwapsEnv era
  , State (EraRule "SWAPS" era) ~ LedgerState era
  , Signal (EraRule "SWAPS" era) ~ Tx era
  , Embed (EraRule "SWAPS" era) (BabelLEDGER era)
  , EraTx era
  , ConwayEraTxBody era
  , BabelEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "LEDGER" BabelUtxosPredFailure era
  , InjectRuleFailure "LEDGER" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  ) =>
  STS (BabelLEDGER era)
  where
  type Environment (BabelLEDGER era) = Shelley.LedgerEnv era
  type PredicateFailure (BabelLEDGER era) = BabelLedgerPredFailure era
  type Signal (BabelLEDGER era) = Tx era
  type State (BabelLEDGER era) = LedgerState era
  type BaseM (BabelLEDGER era) = ShelleyBase
  type Event (BabelLEDGER era) = BabelLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition]

{- CIP-0118#SWAPS-rule

This is an implementation of the Babel fees Agda spec for the SWAPS rule.

We check that the sum of the size of all transactions within the zone is less than
the maximum size of an individual transaction:

`runTestOnSignal $ validateMaxTxSizeUTxO pParams (Foldable.toList txs)`

We then check that all `RequiredTx`s of each transaction in the zone exists as a transaction
in the zone:

`runTestOnSignal $ failureUnless (all (chkRqTx txs) txs) CheckRqTxFailure`

Next, we check that no cycles exist within the dependencies:

`runTestOnSignal $ failureUnless (chkLinear (Foldable.toList txs)) CheckLinearFailure`

Finally, we check that the `ExUnit`s limit is not exceeded:

`runTestOnSignal $ validateExUnitsTooBigUTxO pParams (Foldable.toList txs)`

If these checks pass, we proceed to the LEDGERS rule. Note that, at this point,
we create a `LedgerState` with an empty `FRxO` set.

Please see CIP-0118#ledger-state-temp for more information on `LedgerState`.

Jump to CIP-0118#LEDGERS-rule to continue... -}
ledgerTransition ::
  forall era.
  ( EraRule "LEDGER" era ~ BabelLEDGER era
  , Environment (EraRule "SWAPS" era) ~ BabelSwapsEnv era
  , State (EraRule "SWAPS" era) ~ LedgerState era
  , Signal (EraRule "SWAPS" era) ~ Tx era
  , Embed (EraRule "SWAPS" era) (BabelLEDGER era)
  , ConwayEraTxBody era
  , AlonzoEraUTxO era
  , InjectRuleFailure "LEDGER" BabelUtxosPredFailure era
  , InjectRuleFailure "LEDGER" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  , BabelEraTx era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  ) =>
  TransitionRule (BabelLEDGER era)
ledgerTransition =
  judgmentContext
    >>= \( TRC
            ( LedgerEnv slot ixStart pp account
              , ls@(LedgerState (UTxOState utxo _ _ _ _ _) certState)
              , tx
              )
          ) -> do
        let subTxs = getSubTxs tx
            subTxBodies = getSubTxBodies subTxs
            parentTxBody = tx ^. bodyTxL
            balanceCorInputs txb = balance (UTxO $ unUTxO utxo `Map.restrictKeys` (txb ^. corInputsTxBodyL))
            consumedValue =
              foldMap
                (\tx' -> consumed pp certState utxo tx' <+> balanceCorInputs tx')
                (parentTxBody : subTxBodies)
            producedValue = foldMap (produced pp certState) (parentTxBody : subTxBodies)
            balanced = consumedValue == producedValue
            batchData = mkBatchData balanced (tx : subTxs)
            -- insOK = chkCorIns (body ∷ txBods) (body . TxBody.corInputs)
            insOK = chkCorIns (parentTxBody : subTxBodies) (parentTxBody ^. corInputsTxBodyL)
            -- insOutsOK = chkCorInsOuts (body ∷ txBods) (utx ∣ corInputs)
            insOutsOK =
              chkCorInsOuts
                (parentTxBody : subTxBodies)
                (UTxO $ unUTxO utxo `Map.restrictKeys` (parentTxBody ^. corInputsTxBodyL))
            -- insInUTxO = chkInsInUTxO txBods (dom utx)
            insInUTxO = chkInsInUTxO (parentTxBody : subTxBodies) (Map.keysSet (unUTxO utxo))
            allScripts = foldr (\t l -> (t ^. witsTxL . scriptTxWitsL) `Map.union` l) mempty (tx : subTxs)

        {-   feesOK pp tx utxo   -}
        validate $ feesOK pp tx utxo

        -- we seem to NOT need this one: ∙ batchValid ≡ foldr (λ p q → q ∧ (p .Tx.isValid)) true txs --7
        -- consumed pp u (body ∷ txBods) ≡ produced pp u (body ∷ txBods)

        -- insOK × insOutsOK × insInUTxO
        runTest $
          insOK -- chkCorIns (body ∷ txBods) (body .TxBody.corInputs )
            <> insOutsOK -- chkCorInsOuts (body ∷ txBods) (utx ∣ corInputs)
            <> insInUTxO -- chkInsInUTxO txBods (dom utx)

        -- singleInvalid bd txs ≡ false → balanced
        runTest $
          whenFailure_
            (failureUnless (singleInvalid batchData subTxs) CheckSingleInvalidFailure)
            ( \e ->
                first
                  (<> e)
                  ( failureUnless
                      balanced
                      (ValueNotConservedUTxO consumedValue producedValue)
                  )
            )

        -- lengthˢ subTxIds ≡ length subTxs
        runTest $ chkSubTxsNotRepeated tx
        -- getIDs subTxs ≡ subTxIds
        runTest $ chkSubTxsMatchingTopLevel tx

        -- Assuming TX's size is its own size plus the sum of all its subTxs sizes.
        -- txsize ≤ maxTxSize
        runTestOnSignal $ validateMaxTxSizeUTxO pp tx

        foldM
          ( \ !ls' (ix, tx') ->
              trans @(EraRule "SWAPS" era) $
                TRC
                  ( BabelSwapsEnv
                      slot
                      ix
                      pp
                      account
                      (tx ^. bodyTxL . requireBatchObserversTxBodyL)
                      batchData
                      allScripts
                  , ls'
                  , tx'
                  )
          )
          ls
          $ zip [ixStart ..] (tx : subTxs)
  where
    chkSubTxsNotRepeated :: Tx era -> Test (BabelUtxoPredFailure era)
    chkSubTxsNotRepeated tx = failureUnless (length (txIdsInTopLevel tx) == length (txIdsInBody tx)) CheckSubsNotRepeated
    chkSubTxsMatchingTopLevel :: Tx era -> Test (BabelUtxoPredFailure era)
    chkSubTxsMatchingTopLevel tx = failureUnless (sort (txIdsInTopLevel tx) == sort (txIdsInBody tx)) CheckSubTxsValidFailure
    txIdsInTopLevel tx = tx ^.. subTxTxL . to strictMaybeToMaybe . _Just . folded . to txIdTx
    txIdsInBody tx = tx ^.. bodyTxL . swapsTxBodyL . folded

    -- all corInputs and inputs are the UTxO
    -- chkInsInUTxO : List TxBody → ℙ TxIn → Set
    -- chkInsInUTxO txbods uins = foldr (λ t l → (t .TxBody.txins ∪ t .TxBody.corInputs ⊆ uins) × l) (true ≡ true) txbods
    chkInsInUTxO :: [TxBody era] -> Set (TxIn (EraCrypto era)) -> Test (BabelUtxoPredFailure era)
    chkInsInUTxO txBodies uins = failureUnless check CheckInsInUtxoFailure
      where
        check =
          all
            ( \txBody ->
                ((txBody ^. inputsTxBodyL) `Set.union` (txBody ^. corInputsTxBodyL))
                  `Set.isSubsetOf` uins
            )
            txBodies
    -- all corInputs exist in the UTxO set
    -- chkCorIns : List TxBody → ℙ TxIn → Set
    -- chkCorIns txbods cins = foldr (λ t l → (t .TxBody.txins ∩ cins ≢ ∅) × l) (true ≡ true) txbods
    -- chkCorIns :: [TxBody era] -> Set (TxIn (EraCrypto era)) -> Test (BabelUtxoPredFailure era)
    -- chkCorIns txBodies corInputs = failureUnless check CheckCorInsFailure
    --   where
    --     check =
    --       all
    --         ( \txBody ->
    --             (txBody ^. corInputsTxBodyL) `Set.isSubsetOf` corInputs
    --         )
    --         txBodies

    chkCorIns :: [TxBody era] -> Set (TxIn (EraCrypto era)) -> Test (BabelUtxoPredFailure era)
    chkCorIns txBodies corInputs = failureUnless check CheckCorInsFailure
      where
        check =
          all
            ( \txBody ->
                (txBody ^. corInputsTxBodyL) `Set.intersection` corInputs == mempty -- TODO WG Check with Polina that this is right (since it changed from the above)
            )
            txBodies
    -- check ins in top-level tx correspond to spendOuts in the UTxO set
    -- could do this instead by forcing explicit indexing, ie spendOuts : TxIn - TxOut
    -- chkCorInsOuts : List TxBody → UTxO → Set
    -- chkCorInsOuts tbl uu = compareLists (foldr (_++_) [] (map (λ p → (map proj₂ (setToList (proj₁ (p .TxBody.spendOuts))))) tbl))  (map proj₂ (setToList (proj₁ uu)))
    chkCorInsOuts :: [TxBody era] -> UTxO era -> Test (BabelUtxoPredFailure era)
    chkCorInsOuts tbl uu = failureUnless check CheckCorInsOutsFailure
      where
        check =
          compareLists
            (concatMap (\p -> sizedValue <$> nub (toList (p ^. spendOutsTxBodyL))) tbl)
            (nub (toList (Map.elems $ unUTxO uu)))
        -- do two lists have the same elements?
        -- TODO WG: Obviously this is ridiculously inefficient and will need to be done differently. I might not have time to come back to this though.
        compareLists :: [TxOut era] -> [TxOut era] -> Bool
        compareLists [] [] = True
        compareLists _ [] = False
        compareLists [] _ = False
        compareLists (a : l1) (b : l2) = (a `elem` (b : l2)) && compareLists l1 (deleteFirst a (b : l2))
        deleteFirst :: TxOut era -> [TxOut era] -> [TxOut era]
        deleteFirst _ [] = []
        deleteFirst a (b : bc) =
          if a == b
            then bc
            else b : deleteFirst a bc

getSubTxs :: BabelEraTx era => Tx era -> [Tx era]
getSubTxs tx = tx ^.. subTxTxL . to strictMaybeToMaybe . _Just . folded

getSubTxBodies :: (Foldable f, EraTx era) => f (Tx era) -> [TxBody era]
getSubTxBodies subTxs = subTxs ^.. folded . bodyTxL

-- Probably outdated, keeping for visibility to avoid having to dig back through commits
-- mkBatchData :: AlonzoEraTx era => Tx era -> [Tx era] -> TxId (EraCrypto era) -> BatchData era
-- mkBatchData tx subTxs parentTxId =
--   if not (null subTxs)
--     then Batch parentTxId (IsValid (all (== IsValid True) ((tx : subTxs) ^.. folded . isValidTxL)))
--     else NormalTransaction

normalInvalid :: AlonzoEraTx era => BatchData era -> [Tx era] -> Bool
normalInvalid NormalTransaction [tx] = case tx ^. isValidTxL of
  (IsValid x) -> not x
normalInvalid _ _ = False

mkBatchData :: (AlonzoEraTx era, BabelEraTxBody era) => Bool -> [Tx era] -> BatchData era
mkBatchData _ [] = NormalTransaction -- should not happen
mkBatchData isBalanced [tx] = if noNewFeatures isBalanced (tx ^. bodyTxL) then OldTransaction else NormalTransaction
mkBatchData _ (tx : txs) =
  Batch
    (txIdTx tx)
    (foldr (\p (IsValid q) -> IsValid (q && unIsValid (p ^. isValidTxL))) (IsValid True) (tx : txs))
  where
    unIsValid (IsValid b) = b

noNewFeatures :: BabelEraTxBody era => Bool -> TxBody era -> Bool
noNewFeatures isBalanced txb = case (
                                      (
                                        ( (isBalanced, Set.toList (txb ^. requireBatchObserversTxBodyL))
                                        , toList (txb ^. spendOutsTxBodyL)
                                        )
                                      , toList (txb ^. corInputsTxBodyL)
                                      )
                                    , toList (txb ^. swapsTxBodyL)
                                    ) of
  ((((true, []), []), []), []) -> true
  _ -> False

-- if tx is balanced, and all new features are empty, this is true
-- noNewFeatures : Bool → TxBody → Bool
-- noNewFeatures isBalanced txb with ((((isBalanced , setToList (txb .TxBody.requireBatchObservers)) , setToList (proj₁ (txb .TxBody.spendOuts))) , setToList (txb .TxBody.corInputs  )) , setToList (txb .TxBody.subTxIds ))
-- ... | ((((true , []) , [] ) , []) , []) = true
-- ... | _ = false

-- mkBatchData : Bool → List Tx → BatchData
-- mkBatchData  _ [] = SingularTransaction -- should not happen
-- mkBatchData isBalanced (tx ∷ []) with (noNewFeatures isBalanced (tx .Tx.body))
-- ... | false = SingularTransaction
-- ... | true  = OldTransaction
-- mkBatchData _ (tx ∷ txs) = BatchParent (tx .Tx.body .TxBody.txid) (foldr (λ p q → q ∧ (p .Tx.isValid)) true (tx ∷ txs))

singleInvalid :: AlonzoEraTx era => BatchData era -> [Tx era] -> Bool
singleInvalid NormalTransaction [tx] = (tx ^. isValidTxL) /= IsValid True
singleInvalid _ _ = False

-- ∙ isBalanced ≡ true  → singleInvalid bd txs ≡ false  --2
-- ∙ chkCorIns (body ∷ txBods) (body .TxBody.corInputs ) → singleInvalid bd txs ≡ false --6
-- ∙ chkCorInsOuts (body ∷ txBods) (utx ∣ corInputs) → singleInvalid bd txs ≡ false  --7
-- (isBalanced ≡ true && chkCorIns (body ∷ txBods) (body .TxBody.corInputs ) && chkCorInsOuts (body ∷ txBods) (utx ∣ corInputs)) || (singleInvalid bd txs ≡ false)

-- sameEls : ForTopLevel → List TxId → Set
-- sameEls subTxs lst with subTxs
-- ... | isTopLevel tids = (length (setToList tids) ≡ length lst) × ( tids ≡ (fromList lst))
-- ... | isSubTx = false ≡ true

feesOK ::
  forall era rule.
  ( EraUTxO era
  , InjectRuleFailure rule AlonzoUtxoPredFailure era
  , InjectRuleFailure rule BabbageUtxoPredFailure era
  , InjectRuleFailure rule BabelUtxoPredFailure era
  , ConwayEraTxBody era
  , BabelEraTx era
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
      theFee =
        -- Coin supplied to pay fees
        foldMap
          (^. feeTxBodyL)
          (tx ^. bodyTxL : tx ^.. subTxTxL . to strictMaybeToMaybe . _Just . folded . bodyTxL)
      minFee = getMinFeeTxUtxo pp tx u
   in sequenceA_
        [ -- Part 1: minfee pp tx ≤ txfee txBody
          failureUnless (minFee <= theFee) (injectFailure $ FeeTooSmallUTxO minFee theFee)
        , -- Part 2: (txrdmrs tx ≠ ∅ ⇒ validateCollateral)
          unless (nullRedeemers $ tx ^. witsTxL . rdmrsTxWitsL) $
            validateTotalCollateral pp txBody utxoCollateral
        ]

validateValueNotConservedUTxO ::
  (EraUTxO era, Value era ~ MaryValue (EraCrypto era), BabelEraTxBody era) =>
  PParams era ->
  UTxO era ->
  CertState era ->
  [TxBody era] ->
  Test (BabelUtxoPredFailure era)
validateValueNotConservedUTxO pp utxo certState txs =
  trace
    ( "\n\n Consumed: "
        <> show consumedValue
        <> " | Produced: "
        <> show producedValue
        <> " \n Num Txs in zone: "
        <> show (length txs)
        -- <> " \n The UTXO: "
        -- <> show utxo
    )
    failureUnless
    (consumedValue == producedValue)
    $ ValueNotConservedUTxO consumedValue producedValue
  where
    -- consumed :: PParams → UTxOState → List TxBody → Value
    -- consumed pp st txbls
    --   =  foldr  (λ txb → (let open TxBody in balance (st .utxo ∣ txb .txins)
    --   +  txb .mint
    --   +  inject (depositRefunds pp st txb)
    --   +  balance (st .utxo ∣ txb .corInputs)) +_) (inject 0) txbls
    balanceCorInputs txb = balance (UTxO $ unUTxO utxo `Map.restrictKeys` (txb ^. corInputsTxBodyL))

    consumedValue = foldMap (\tx -> consumed pp certState utxo tx <+> balanceCorInputs tx) txs -- <+> the corinputs thing
    producedValue = foldMap (produced pp certState) txs

-- check that collateral in each transaction in the list is enough to cover the preceeding ones
collForPrec ::
  (EraTx era, AlonzoEraTxBody era) =>
  [Tx era] ->
  Integer ->
  UTxO era ->
  Integer ->
  Either (BabelUtxoPredFailure era) ()
collForPrec [] _ _ 0 = Right ()
collForPrec [] _ _ c = Left $ CollForPrecValidFailure (Coin c)
collForPrec (t : l) cp u c =
  let collateralPossessed = unCoin (coin (balance (txInsFilter u (t ^. bodyTxL . collateralInputsTxBodyL))))
   in do
        unless (c <= collateralPossessed) (Left $ CollForPrecValidFailure (Coin $ c - collateralPossessed))
        collForPrec l cp u (c - (unCoin (t ^. bodyTxL . feeTxBodyL) * cp))

collInUTxO :: (EraTx era, AlonzoEraTxBody era) => [Tx era] -> UTxO era -> Bool
collInUTxO [] _ = True
collInUTxO (t : l) utxo@(UTxO u) =
  ((t ^. bodyTxL . collateralInputsTxBodyL) `Set.isSubsetOf` Set.fromList (Map.keys u))
    && collInUTxO l utxo

txInTxId :: TxIn c -> TxId c
txInTxId (TxIn x _) = x

-- get a set of TxIds containing all IDs of transaction in given list tb
getIDs :: EraTx era => [Tx era] -> Set (TxId (EraCrypto era))
getIDs = foldr (\tx ls -> ls `Set.union` Set.singleton (txIdTx tx)) mempty

instance
  ( Era era
  , STS (BabelSWAPS era)
  , PredicateFailure (EraRule "SWAPS" era) ~ BabelSwapsPredFailure era
  , Event (EraRule "SWAPS" era) ~ BabelSwapsEvent era
  ) =>
  Embed (BabelSWAPS era) (BabelLEDGER era)
  where
  wrapFailed = SwapsFailure
  wrapEvent = SwapsEvent