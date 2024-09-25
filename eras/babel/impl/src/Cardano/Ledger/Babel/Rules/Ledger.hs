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
import Cardano.Ledger.Val (coin)
import Control.DeepSeq (NFData)
import Control.Monad (foldM, unless, when)
import Control.Monad.RWS (asks)
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition (validate)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (foldl'), sequenceA_, toList)
import Data.Function ((&))
import Data.List (sort)
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
        let subTxs = tx ^.. subTxTxL . to strictMaybeToMaybe . _Just . folded
            subTxBodies = subTxs ^.. folded . bodyTxL
            parentTxBody = tx ^. bodyTxL
            parentTxId = txIdTx tx
            batchData =
              if not (null subTxs)
                then Batch parentTxId (IsValid (all (== IsValid True) ((tx : subTxs) ^.. folded . isValidTxL)))
                else NormalTransaction
        {-   feesOK pp tx utxo   -}
        validate $ feesOK pp tx utxo

        -- we seem to NOT need this one: ∙ batchValid ≡ foldr (λ p q → q ∧ (p .Tx.isValid)) true txs --7
        -- consumed pp u (body ∷ txBods) ≡ produced pp u (body ∷ txBods)

        let batchChecks =
              validateValueNotConservedUTxO
                pp
                utxo
                certState
                (parentTxBody : subTxBodies)
                <> chkCorIns -- chkCorIns (body ∷ txBods) (body .TxBody.corInputs )
                  (parentTxBody : subTxBodies)
                  (parentTxBody ^. corInputsTxBodyL)
                -- chkCorInsOuts (body ∷ txBods) (utx ∣ corInputs)
                <> chkCorInsOuts
                  (parentTxBody : subTxBodies)
                  (UTxO $ unUTxO utxo `Map.restrictKeys` (parentTxBody ^. corInputsTxBodyL))

        runTest $
          whenFailure_
            batchChecks
            ( \e ->
                first
                  (<> e)
                  (failureUnless (singleInvalid batchData subTxs) CheckSingleInvalidFailure)
            )

        -- whenFailure
        runTest $
          chkSubTxsUniqueAndMatchingTopLevel tx

        -- Assuming TX's size is its own size plus the sum of all its subTxs sizes
        runTestOnSignal $ validateMaxTxSizeUTxO pp tx
        -- chkInsInUTxO txBods (dom utx)
        runTestOnSignal $ chkInsInUTxO (parentTxBody : subTxBodies) (Map.keysSet (unUTxO utxo))
        -- sameEls subTxs (map (λ t → t .Tx'.body' .TxBody.txid) subTxBodies)
        -- runTestOnSignal $
        --   failureUnless
        --     (sameEls subTxs (map (λ t → t ^. bodyTxL . txidTxBodyL) subTxBodies))
        --     CheckSameElsFailure
        foldM
          ( \ !ls' (ix, tx') ->
              trans @(EraRule "SWAPS" era) $
                TRC
                  ( BabelSwapsEnv slot ix pp account (tx ^. bodyTxL . requireBatchObserversTxBodyL) batchData
                  , ls'
                  , tx'
                  )
          )
          ls
          $ zip [ixStart ..] (tx : subTxs)
  where
    chkSubTxsUniqueAndMatchingTopLevel :: Tx era -> Test (BabelUtxoPredFailure era)
    chkSubTxsUniqueAndMatchingTopLevel tx = failureUnless (sort txIdsInTopLevel == sort txIdsInBody) CheckSubTxsValidFailure
      where
        txIdsInTopLevel = tx ^.. subTxTxL . to strictMaybeToMaybe . _Just . folded . to txIdTx
        txIdsInBody = tx ^.. bodyTxL . swapsTxBodyL . folded
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
    chkCorIns :: [TxBody era] -> Set (TxIn (EraCrypto era)) -> Test (BabelUtxoPredFailure era)
    chkCorIns txBodies corInputs = failureUnless check CheckCorInsFailure
      where
        check =
          all
            ( \txBody ->
                (txBody ^. corInputsTxBodyL) `Set.isSubsetOf` corInputs
            )
            txBodies
    -- check ins in top-level tx correspond to spendOuts in the UTxO set
    -- could do this instead by forcing explicit indexing, ie spendOuts : TxIn - TxOut
    chkCorInsOuts :: [TxBody era] -> UTxO era -> Test (BabelUtxoPredFailure era)
    chkCorInsOuts tbl uu = failureUnless check CheckCorInsOutsFailure
      where
        check =
          compareLists
            (concatMap (\p -> sizedValue <$> toList (p ^. spendOutsTxBodyL)) tbl)
            (toList (Map.elems $ unUTxO uu))
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

-- judgmentContext
--   >>= \( TRC
--           ( UtxoEnv slotNo pParams accountState
--             , LedgerState utxoState certState
--             , txs :: Tx era
--             )
--         ) -> do
--       let ltx = Foldable.toList txs
--           lsV = init ltx
--           tx = last ltx -- TODO WG use safe head
--           collateralPct = collateralPercentage pParams
--           utxo@(UTxO u) = utxoState ^. utxosUtxoL

--       {- ((totSizeZone ltx) ≤ᵇ (Γ .LEnv.pparams .PParams.maxTxSize)) ≡ true -}
--       runTestOnSignal $
--         validateMaxTxSizeUTxO pParams ltx
--       -- ((coin (balance  (utxo ∣ tx .body .collateral)) * 100) ≥ᵇ sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ true

--       -- TODO WG MIDGROUND ADD THESE CHECKS
--       -- the sum total of the fees of all transactions in the zone is at least the sum of the required fees for all transactions in the zone (note that there are changes to fee and collateral requirements, discussed below)
--       if all chkIsValid txs -- SWAPS-V
--         then do
--           -- ∪_{tx ∈ txs} txins(tx) ⊆ dom utxo
--           runTestOnSignal $
--             failureUnless
--               (all (`member` u) $ Foldable.toList =<< ltx ^.. folded . bodyTxL . inputsTxBodyL)
--               DependsOnZoneOutput

--           {- totExunits tx ≤ maxTxExUnits pp -}
--           runTestOnSignal $ validateExUnitsTooBigUTxO pParams ltx

--           {- collForPrec ltx (Γ .LEnv.pparams .PParams.collateralPercentage) utxo (sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ just _ -}
--           let res =
--                 collForPrec
--                   (reverse ltx)
--                   collateralPct
--                   utxo
--                   (unCoin $ sumCol ltx collateralPct)
--           case res of
--             Left e -> runTestOnSignal $ failure e
--             _ -> pure ()

--           {- collInUTxO ltx utxo -}
--           runTestOnSignal $
--             failureUnless (collInUTxO ltx utxo) CollInUtxoValidFailure

--           {- consumed pp utxo txb = produced pp poolParams txb -}
--           runTest $
--             validateValueNotConservedUTxO pParams utxo certState (Foldable.toList (txs ^.. folded . bodyTxL))

--           utxoState' <-
--             trans @(EraRule "UTXOW" era) $
--               TRC
--                 ( UtxoEnv slotNo pParams accountState
--                 , utxoState
--                 , txs
--                 )
--           pure (LedgerState utxoState' certState)
--         else -- SWAPS-N
--         do
--           -- Check that only the last transaction is invalid
--           runTestOnSignal $
--             failureUnless (chkExactlyLastInvalid ltx) MoreThanOneInvalidTransaction

--           {- collForPrec ltx (Γ .LEnv.pparams .PParams.collateralPercentage) utxo (sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ just _ -}
--           let res =
--                 collForPrec
--                   (reverse (lsV ++ [tx]))
--                   collateralPct
--                   utxo
--                   (unCoin $ sumCol (lsV ++ [tx]) collateralPct)

--           case res of
--             Left e -> runTestOnSignal $ failure e
--             _ -> pure ()

--           {- collInUTxO (lsV ++ [ tx ]) utxo -}
--           runTestOnSignal $
--             failureUnless
--               (collInUTxO (lsV ++ [tx]) utxo)
--               CollInUtxoInvalidFailure

--           babelEvalScriptsTxInvalid @era
-- where
--   -- chkIsValid tx = tx .Tx.isValid ≡ true
--   chkIsValid :: Tx era -> Bool
--   chkIsValid tx = tx ^. isValidTxL == IsValid True
--   sizeTx :: Tx era -> Integer
--   sizeTx t = t ^. sizeTxF
--   totSizeZone :: [Tx era] -> Integer
--   totSizeZone z = sum (map sizeTx z)
--   validateMaxTxSizeUTxO ::
--     PParams era ->
--     [Tx era] ->
--     Test (BabelUtxoPredFailure era)
--   validateMaxTxSizeUTxO pp z =
--     failureUnless (zoneSize <= maxTxSize) $ MaxTxSizeUTxO zoneSize maxTxSize
--     where
--       maxTxSize = toInteger (pp ^. ppMaxTxSizeL)
--       zoneSize = totSizeZone z
--   validateExUnitsTooBigUTxO ::
--     PParams era ->
--     [Tx era] ->
--     Test (BabelUtxoPredFailure era)
--   validateExUnitsTooBigUTxO pp txs =
--     failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
--       ExUnitsTooBigUTxO maxTxExUnits totalExUnits
--     where
--       maxTxExUnits = pp ^. ppMaxTxExUnitsL
--       -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the zone:
--       totalExUnits = Foldable.foldl' (<>) mempty $ fmap totExUnits txs
--   chkExactlyLastInvalid :: [Tx era] -> Bool
--   chkExactlyLastInvalid txs = case reverse txs of
--     (l : txs') -> (l ^. isValidTxL == IsValid False) && all ((== IsValid True) . (^. isValidTxL)) txs'
--     [] -> True
--   collateralPercentage pParams = toInteger $ pParams ^. ppCollateralPercentageL
--   sumCol :: [Tx era] -> Integer -> Coin
--   sumCol tb cp = Coin $ foldr (\tx c -> c + (unCoin (tx ^. bodyTxL . feeTxBodyL) * cp)) 0 tb

validateValueNotConservedUTxO ::
  (EraUTxO era, Value era ~ MaryValue (EraCrypto era)) =>
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
    consumedValue = foldMap (consumed pp certState utxo) txs -- <+> the corinputs thing
    producedValue = foldMap (produced pp certState) txs

babelEvalScriptsTxInvalid ::
  forall era.
  ( EraRule "LEDGER" era ~ BabelLEDGER era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , Environment (EraRule "SWAPS" era) ~ BabelSwapsEnv era
  , State (EraRule "SWAPS" era) ~ LedgerState era
  , Signal (EraRule "SWAPS" era) ~ Tx era
  , Embed (EraRule "SWAPS" era) (BabelLEDGER era)
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraRuleFailure "LEDGER" era ~ BabelSwapsPredFailure era
  , InjectRuleFailure "LEDGER" BabelUtxosPredFailure era
  , InjectRuleFailure "LEDGER" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  , Eq (PredicateFailure (EraRule "UTXO" era))
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  TransitionRule (BabelSWAPS era)
babelEvalScriptsTxInvalid = undefined

-- do
--   TRC
--     ( UtxoEnv _slotNo pp _accountState
--       , LedgerState us@(UTxOState utxo _ fees _ _ _) certState
--       , txs :: Tx era
--       ) <-
--     judgmentContext
--   -- TODO WG: Is the list last first or last...last (Probably last last)
--   let tx = last (Foldable.toList txs) -- TODO WG use safe head
--       txBody = tx ^. bodyTxL

--   -- {- txb := txbody tx -}
--   sysSt <- liftSTS $ asks systemStart
--   ei <- liftSTS $ asks epochInfo

--   () <- pure $! traceEvent invalidBegin ()

--   -- TODO WG Should this script collection even happen here (obviously collat needs collecting but is this too much?)?
--   {- TODO WG:
--     I think you actually need a different function that collects Plutus scripts from
--     ALL transactions, but just using the collateral for the last one? Or evals scripts from ALL txs? Or something like that?
--     Basically, yes, the last TX is the one that failed, but we need to collect collat for all the other ones, too. -}
--   case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
--     Right sLst ->
--       {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
--       {- isValid tx = evalScripts tx sLst = False -}
--       whenFailureFree $
--         when2Phase $ case evalPlutusScripts tx sLst of
--           Passes _ ->
--             failBecause $
--               injectFailure @"SWAPS" $
--                 ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
--           Fails ps fs -> do
--             mapM_ (tellEvent . ZoneSuccessfulPlutusScriptsEvent @era) (nonEmpty ps)
--             tellEvent (ZoneFailedPlutusScriptsEvent @era (scriptFailurePlutus <$> fs))
--     Left info -> failBecause (injectFailure $ CollectErrors info)
--   () <- pure $! traceEvent invalidEnd ()

--   {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
--   {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
--   let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
--       UTxO collouts = collOuts txBody
--       DeltaCoin collateralFees = collAdaBalance txBody utxoDel
--   pure $!
--     LedgerState
--       us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
--         { utxosUtxo = UTxO (Map.union utxoKeep collouts)
--         , {- fees + collateralFees -}
--           utxosFees = fees <> Coin collateralFees
--         , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
--         }
--       certState

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
  -- trace
  --   ( "\n\n Collateral we need: "
  --       <> show c
  --       <> "\n\n Collateral we have: "
  --       <> show (unCoin (coin (balance (txInsFilter u (t ^. bodyTxL . collateralInputsTxBodyL)))))
  --       <> "\n\n Num TXs: "
  --       <> show (length (t : l))
  --       <> "\n\n Collateral Percentage: "
  --       <> show cp
  --   )
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