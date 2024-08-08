{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Cardano.Ledger.Babel.Rules.Zone where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx)
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), totExUnits)
import Cardano.Ledger.Babel.Core (
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  InjectRuleFailure (..),
  collateralInputsTxBodyL,
  isValidTxL,
 )
import Cardano.Ledger.Babel.Era (BabelEra, BabelZONE)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  epochInfo,
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
import Lens.Micro (folded, (^.), (^..))

import Cardano.Ledger.Allegra.Core (EraTxBody (..))
import Cardano.Ledger.Alonzo.Core (ppCollateralPercentageL)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext, evalPlutusScripts)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (ValidationTagMismatch),
  TagMismatchDescription (PassedUnexpectedly),
  invalidBegin,
  invalidEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babel.Core (AlonzoEraTxBody, Value, ppMaxTxExUnitsL)
import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Ledgers (BabelLEDGERS, BabelLedgersEnv (BabelLedgersEnv))
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure (..))
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Binary.Decoding (DecCBOR (..))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody)
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus (
  PlutusWithContext,
  ScriptFailure (scriptFailurePlutus),
  ScriptResult (..),
 )
import Cardano.Ledger.Plutus.ExUnits (pointWiseExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (updateStakeDistribution, utxosUtxoL)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
 )
import Cardano.Ledger.Shelley.UTxO (consumed, produced)
import Cardano.Ledger.UTxO (EraUTxO (ScriptsNeeded), balance, txInsFilter)
import Cardano.Ledger.Val (coin)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.RWS (asks)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (trace, traceEvent)
import NoThunks.Class (NoThunks)
import Validation (failure, failureUnless)

data BabelZonePredFailure era
  = LedgersFailure (PredicateFailure (BabelLEDGERS era)) -- Subtransition Failures
  | ShelleyInBabelPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
  deriving (Generic)

data BabelZoneEvent era
  = ShelleyInBabelEvent (ShelleyLedgersEvent era)
  | ZoneFailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | ZoneSuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  deriving (Generic)

type instance EraRuleFailure "ZONE" (BabelEra c) = BabelZonePredFailure (BabelEra c)

instance InjectRuleFailure "ZONE" BabelZonePredFailure (BabelEra c)

type instance EraRuleEvent "ZONE" (BabelEra c) = BabelZoneEvent (BabelEra c)

instance InjectRuleFailure "ZONE" ShelleyLedgersPredFailure (BabelEra c) where
  injectFailure = LedgersFailure

instance InjectRuleFailure "ZONE" BabelLedgerPredFailure (BabelEra c) where
  injectFailure :: BabelLedgerPredFailure (BabelEra c) -> BabelZonePredFailure (BabelEra c)
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = LedgersFailure . injectFailure

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (BabelZonePredFailure era)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (BabelZonePredFailure era)

deriving anyclass instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (BabelZonePredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "LEDGER" era))
  , NFData (ShelleyLedgersPredFailure era)
  ) =>
  NFData (BabelZonePredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  EncCBOR (BabelZonePredFailure era)
  where
  encCBOR =
    encode . \case
      LedgersFailure x -> Sum (LedgersFailure @era) 1 !> To x
      ShelleyInBabelPredFailure x -> Sum (ShelleyInBabelPredFailure @era) 2 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (BabelZonePredFailure era)
  where
  decCBOR =
    decode $ Summands "BabelZonePredFailure" $ \case
      1 -> SumD LedgersFailure <! From
      2 -> SumD ShelleyInBabelPredFailure <! From
      n -> Invalid n

deriving instance
  ( Era era
  , Show (Event (EraRule "LEDGER" era))
  ) =>
  Show (BabelZoneEvent era)

deriving instance
  ( Era era
  , Eq (Event (EraRule "LEDGER" era))
  ) =>
  Eq (BabelZoneEvent era)

deriving anyclass instance
  ( Era era
  , NoThunks (ShelleyLedgersEvent era)
  , NoThunks (PlutusWithContext (EraCrypto era))
  ) =>
  NoThunks (BabelZoneEvent era)

instance
  ( Era era
  , NFData (ShelleyLedgersEvent era)
  ) =>
  NFData (BabelZoneEvent era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  , EncCBOR (Event (EraRule "LEDGER" era))
  , EncCBOR (PlutusWithContext (EraCrypto era))
  ) =>
  EncCBOR (BabelZoneEvent era)
  where
  encCBOR =
    encode . \case
      ShelleyInBabelEvent x -> Sum (ShelleyInBabelEvent @era) 1 !> To x
      ZoneFailedPlutusScriptsEvent x -> Sum (ZoneFailedPlutusScriptsEvent @era) 2 !> To x
      ZoneSuccessfulPlutusScriptsEvent x -> Sum (ZoneSuccessfulPlutusScriptsEvent @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (Event (EraRule "LEDGER" era))
  , DecCBOR (PlutusWithContext (EraCrypto era))
  ) =>
  DecCBOR (BabelZoneEvent era)
  where
  decCBOR =
    decode $ Summands "BabelZonePredFailure" $ \case
      1 -> SumD ShelleyInBabelEvent <! From
      2 -> SumD ZoneFailedPlutusScriptsEvent <! From
      3 -> SumD ZoneSuccessfulPlutusScriptsEvent <! From
      n -> Invalid n

instance
  ( EraRule "ZONE" era ~ BabelZONE era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ConwayEraPParams era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , EraTx era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  STS (BabelZONE era)
  where
  type Environment (BabelZONE era) = BabelLedgersEnv era
  type PredicateFailure (BabelZONE era) = BabelZonePredFailure era
  type Signal (BabelZONE era) = Seq (Tx era)
  type State (BabelZONE era) = LedgerState era
  type BaseM (BabelZONE era) = ShelleyBase
  type Event (BabelZONE era) = BabelZoneEvent era

  initialRules = []
  transitionRules = [zoneTransition]

{- CIP-0118#ZONE-rule

This is an implementation of the Babel fees Agda spec for the ZONE rule.

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
zoneTransition ::
  forall era.
  ( EraRule "ZONE" era ~ BabelZONE era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TransitionRule (BabelZONE era)
zoneTransition =
  judgmentContext
    >>= \( TRC
            ( BabelLedgersEnv slotNo ixStart pParams accountState
              , LedgerState utxoState certState
              , txs :: Seq (Tx era)
              )
          ) -> do
        let ltx = Foldable.toList txs
            lsV = init ltx
            tx = last ltx -- TODO WG use safe head
            collateralPct = collateralPercentage pParams
            utxo = utxoState ^. utxosUtxoL

        {- ((totSizeZone ltx) ≤ᵇ (Γ .LEnv.pparams .PParams.maxTxSize)) ≡ true -}
        runTestOnSignal $
          validateMaxTxSizeUTxO pParams ltx
        -- ((coin (balance  (utxo ∣ tx .body .collateral)) * 100) ≥ᵇ sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ true

        if all chkIsValid txs -- ZONE-V
          then do
            {- totExunits tx ≤ maxTxExUnits pp -}
            runTestOnSignal $ validateExUnitsTooBigUTxO pParams ltx

            {- collForPrec ltx (Γ .LEnv.pparams .PParams.collateralPercentage) utxo (sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ just _ -}
            let res =
                  collForPrec
                    (reverse ltx)
                    collateralPct
                    utxo
                    (unCoin $ sumCol ltx collateralPct)
            case res of
              Left e -> runTestOnSignal $ failure e
              _ -> pure ()

            {- collInUTxO ltx utxo -}
            runTestOnSignal $
              failureUnless (collInUTxO ltx utxo) CollInUtxoValidFailure

            {- consumed pp utxo txb = produced pp poolParams txb -}
            runTest $
              validateValueNotConservedUTxO pParams utxo certState (Foldable.toList (txs ^.. folded . bodyTxL))

            trans @(EraRule "LEDGERS" era) $
              TRC
                ( BabelLedgersEnv slotNo ixStart pParams accountState
                , LedgerState utxoState certState
                , txs
                )
          else -- ZONE-N
          do
            -- Check that only the last transaction is invalid
            runTestOnSignal $
              failureUnless (chkExactlyLastInvalid ltx) MoreThanOneInvalidTransaction

            {- collForPrec ltx (Γ .LEnv.pparams .PParams.collateralPercentage) utxo (sumCol ltx (Γ .LEnv.pparams .PParams.collateralPercentage)) ≡ just _ -}
            let res =
                  collForPrec
                    (reverse (lsV ++ [tx]))
                    collateralPct
                    utxo
                    (unCoin $ sumCol (lsV ++ [tx]) collateralPct)

            case res of
              Left e -> runTestOnSignal $ failure e
              _ -> pure ()

            {- collInUTxO (lsV ++ [ tx ]) utxo -}
            runTestOnSignal $
              failureUnless
                (collInUTxO (lsV ++ [tx]) utxo)
                CollInUtxoInvalidFailure

            babelEvalScriptsTxInvalid @era
  where
    -- chkIsValid tx = tx .Tx.isValid ≡ true
    chkIsValid :: Tx era -> Bool
    chkIsValid tx = tx ^. isValidTxL == IsValid True
    sizeTx :: Tx era -> Integer
    sizeTx t = t ^. sizeTxF
    totSizeZone :: [Tx era] -> Integer
    totSizeZone z = sum (map sizeTx z)
    validateMaxTxSizeUTxO ::
      PParams era ->
      [Tx era] ->
      Test (BabelUtxoPredFailure era)
    validateMaxTxSizeUTxO pp z =
      failureUnless (zoneSize <= maxTxSize) $ MaxTxSizeUTxO zoneSize maxTxSize
      where
        maxTxSize = toInteger (pp ^. ppMaxTxSizeL)
        zoneSize = totSizeZone z
    validateExUnitsTooBigUTxO ::
      PParams era ->
      [Tx era] ->
      Test (BabelUtxoPredFailure era)
    validateExUnitsTooBigUTxO pp txs =
      failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
        ExUnitsTooBigUTxO maxTxExUnits totalExUnits
      where
        maxTxExUnits = pp ^. ppMaxTxExUnitsL
        -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the zone:
        totalExUnits = Foldable.foldl' (<>) mempty $ fmap totExUnits txs
    chkExactlyLastInvalid :: [Tx era] -> Bool
    chkExactlyLastInvalid txs = case reverse txs of
      (l : txs') -> (l ^. isValidTxL == IsValid False) && all ((== IsValid True) . (^. isValidTxL)) txs'
      [] -> True
    collateralPercentage pParams = toInteger $ pParams ^. ppCollateralPercentageL
    sumCol :: [Tx era] -> Integer -> Coin
    sumCol tb cp = Coin $ foldr (\tx c -> c + (unCoin (tx ^. bodyTxL . feeTxBodyL) * cp)) 0 tb

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
        <> " \n The UTXO: "
        <> show utxo
    )
    failureUnless
    (consumedValue == producedValue)
    $ ValueNotConservedUTxO consumedValue producedValue
  where
    consumedValue = foldMap (consumed pp certState utxo) txs
    producedValue = foldMap (produced pp certState) txs

babelEvalScriptsTxInvalid ::
  forall era.
  ( EraRule "ZONE" era ~ BabelZONE era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , Environment (EraRule "LEDGERS" era) ~ BabelLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (BabelZONE era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraRuleFailure "ZONE" era ~ BabelZonePredFailure era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , InjectRuleFailure "ZONE" BabelUtxosPredFailure era
  , InjectRuleFailure "ZONE" BabelUtxoPredFailure era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  TransitionRule (BabelZONE era)
babelEvalScriptsTxInvalid =
  do
    TRC
      ( BabelLedgersEnv _slotNo _ixRange pp _accountState
        , LedgerState us@(UTxOState utxo _ fees _ _ _) certState
        , txs :: Seq (Tx era)
        ) <-
      judgmentContext
    -- TODO WG: Is the list last first or last...last (Probably last last)
    let tx = last (Foldable.toList txs) -- TODO WG use safe head
        txBody = tx ^. bodyTxL

    -- {- txb := txbody tx -}
    sysSt <- liftSTS $ asks systemStart
    ei <- liftSTS $ asks epochInfo

    () <- pure $! traceEvent invalidBegin ()

    -- TODO WG Should this script collection even happen here (obviously collat needs collecting but is this too much?)?
    {- TODO WG:
      I think you actually need a different function that collects Plutus scripts from
      ALL transactions, but just using the collateral for the last one? Or evals scripts from ALL txs? Or something like that?
      Basically, yes, the last TX is the one that failed, but we need to collect collat for all the other ones, too. -}
    case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
      Right sLst ->
        {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
        {- isValid tx = evalScripts tx sLst = False -}
        whenFailureFree $
          when2Phase $ case evalPlutusScripts tx sLst of
            Passes _ ->
              failBecause $
                injectFailure @"ZONE" $
                  ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
            Fails ps fs -> do
              mapM_ (tellEvent . ZoneSuccessfulPlutusScriptsEvent @era) (nonEmpty ps)
              tellEvent (ZoneFailedPlutusScriptsEvent @era (scriptFailurePlutus <$> fs))
      Left info -> failBecause (injectFailure $ CollectErrors info)
    () <- pure $! traceEvent invalidEnd ()

    {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
    {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
    let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
        UTxO collouts = collOuts txBody
        DeltaCoin collateralFees = collAdaBalance txBody utxoDel
    pure $!
      LedgerState
        us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
          { utxosUtxo = UTxO (Map.union utxoKeep collouts)
          , {- fees + collateralFees -}
            utxosFees = fees <> Coin collateralFees
          , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
          }
        certState

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

txInTxId :: TxIn era -> TxId era
txInTxId (TxIn x _) = x

-- get a set of TxIds containing all IDs of transaction in given list tb
getIDs :: EraTx era => [Tx era] -> Set (TxId (EraCrypto era))
getIDs = foldr (\tx ls -> ls `Set.union` Set.singleton (txIdTx tx)) mempty

instance
  ( Era era
  , STS (BabelLEDGERS era)
  , PredicateFailure (EraRule "LEDGERS" era) ~ ShelleyLedgersPredFailure era
  ) =>
  Embed (BabelLEDGERS era) (BabelZONE era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInBabelEvent