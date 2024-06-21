{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Cardano.Ledger.Conway.Rules.Zone where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTx)
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), totExUnits)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  epochInfo,
  systemStart,
 )
import Cardano.Ledger.Conway.Core (
  ConwayEraTxBody (fulfillsTxBodyL),
  Era (EraCrypto),
  EraRule,
  EraTx (Tx, bodyTxL),
  EraTxBody (TxBody, inputsTxBodyL),
  InjectRuleFailure (..),
  collateralInputsTxBodyL,
  isValidTxL,
  requiredTxsTxBodyL,
 )
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayZONE)
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
import Data.Set (Set, toList)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.Type (Lens')

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext, evalPlutusScripts)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure (ValidationTagMismatch),
  AlonzoUtxowPredFailure,
  TagMismatchDescription (PassedUnexpectedly),
  invalidBegin,
  invalidEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Conway.Core (ppMaxTxExUnitsL)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers (ConwayLEDGERS, ConwayLedgersEnv (ConwayLedgersEnv))
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure (CollectErrors))
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Plutus (
  PlutusWithContext,
  ScriptFailure (scriptFailurePlutus),
  ScriptResult (..),
 )
import Cardano.Ledger.Plutus.ExUnits (pointWiseExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (updateStakeDistribution)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Cardano.Ledger.UTxO (EraUTxO (ScriptsNeeded))
import Control.Monad.RWS (asks)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import NoThunks.Class (NoThunks)
import Validation (failureUnless)

data ConwayZonePredFailure era
  = LedgersFailure (PredicateFailure (ConwayLEDGERS era)) -- Subtransition Failures
  | -- | ShelleyInConwayPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
    ShelleyInConwayPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
  deriving (Generic)

data ConwayZoneEvent era
  = ShelleyInConwayEvent (ShelleyLedgersEvent era)
  | ZoneFailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | ZoneSuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))

type instance EraRuleFailure "ZONE" (ConwayEra c) = ConwayZonePredFailure (ConwayEra c)

instance InjectRuleFailure "ZONE" ConwayZonePredFailure (ConwayEra c)

type instance EraRuleEvent "ZONE" (ConwayEra c) = ConwayZoneEvent (ConwayEra c)

instance InjectRuleFailure "ZONE" ShelleyLedgersPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure

instance InjectRuleFailure "ZONE" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure :: ConwayLedgerPredFailure (ConwayEra c) -> ConwayZonePredFailure (ConwayEra c)
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "ZONE" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = LedgersFailure . injectFailure

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (ConwayZonePredFailure era)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (ConwayZonePredFailure era)

deriving anyclass instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (ConwayZonePredFailure era)

instance
  ( EraRule "ZONE" era ~ ConwayZONE era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ConwayEraPParams era
  , Environment (EraRule "LEDGERS" era) ~ ConwayLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , EraTx era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" ConwayUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ ConwayZonePredFailure era
  , InjectRuleFailure "ZONE" ConwayUtxoPredFailure era
  ) =>
  STS (ConwayZONE era)
  where
  type Environment (ConwayZONE era) = ConwayLedgersEnv era
  type PredicateFailure (ConwayZONE era) = ConwayZonePredFailure era
  type Signal (ConwayZONE era) = Seq (Tx era)
  type State (ConwayZONE era) = LedgerState era
  type BaseM (ConwayZONE era) = ShelleyBase
  type Event (ConwayZONE era) = ConwayZoneEvent era

  initialRules = []
  transitionRules = [zoneTransition]

{- txsize tx ≤ maxTxSize pp -}
-- We've moved this to the ZONE rule. See https://github.com/IntersectMBO/formal-ledger-specifications/commit/c3e18ac1d3da92dd4894bbc32057a143f9720f52#diff-5f67369ed62c0dab01e13a73f072b664ada237d094bbea4582365264dd163bf9
-- ((totSizeZone ltx) ≤ᵇ (Γ .LEnv.pparams .PParams.maxTxSize)) ≡ true
-- runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

zoneTransition ::
  forall era.
  ( EraRule "ZONE" era ~ ConwayZONE era
  , Environment (EraRule "LEDGERS" era) ~ ConwayLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , EraPlutusContext era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "ZONE" ConwayUtxosPredFailure era
  , PredicateFailure (EraRule "ZONE" era) ~ ConwayZonePredFailure era
  , InjectRuleFailure "ZONE" ConwayUtxoPredFailure era
  ) =>
  TransitionRule (ConwayZONE era)
zoneTransition =
  judgmentContext
    -- I guess we want UTxOStateTemp here?
    >>= \( TRC
            ( ConwayLedgersEnv slotNo ixRange pParams accountState
              , LedgerState utxoState certState
              , txs :: Seq (Tx era)
              )
          ) -> do
        {-   totExunits tx ≤ maxTxExUnits pp
              runTest $ Alonzo.validateExUnitsTooBigUTxO pp tx -}
        runTestOnSignal $ validateMaxTxSizeUTxO pParams (Foldable.toList txs)
        if all chkIsValid txs -- ZONE-V
          then do
            -- TODO WG: make sure `runTestOnSignal` is correct rather than `runTest`
            runTestOnSignal $ failureUnless (all (chkRqTx txs) txs) CheckRqTxFailure
            runTestOnSignal $ failureUnless (chkLinear (Foldable.toList txs)) CheckLinearFailure
            runTestOnSignal $ validateExUnitsTooBigUTxO pParams (Foldable.toList txs)
            trans @(EraRule "LEDGERS" era) $
              TRC (ConwayLedgersEnv slotNo ixRange pParams accountState, LedgerState utxoState certState, txs)
          else -- Add failure condition on anything other than: exactly 1 invalid (last tx)
          -- ZONE-N
          do
            runTestOnSignal $
              failureUnless (chkExactlyLastInvalid (Foldable.toList txs)) MoreThanOneInvalidTransaction
            conwayEvalScriptsTxInvalid @era
  where
    chkLinear :: [Tx era] -> Bool
    chkLinear txs =
      topSortTxs
        (mkAllEdges txs txs)
        (mkAllEdges txs txs)
        (nodesWithNoIncomingEdge txs (mkAllEdges txs txs))
        []
        == Just []
    -- chkRqTx txs tx = ∀[ txrid ∈ tx .Tx.body .TxBody.requiredTxs ] Any (txrid ≡_) ( getIDs txs )
    chkRqTx :: Seq (Tx era) -> Tx era -> Bool
    chkRqTx txs tx = all chk txrids
      where
        chk txrid = txrid `elem` ids
        -- asd = tx ^. requiredTxsTxL
        txrids = fmap txInTxId $ toList $ tx ^. bodyTxL . requiredTxsTxBodyL
        ids :: Set (TxId (EraCrypto era))
        ids = getIDs $ Foldable.toList txs
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
      Test (ConwayUtxoPredFailure era)
    validateMaxTxSizeUTxO pp z =
      failureUnless (zoneSize <= maxTxSize) $ MaxTxSizeUTxO zoneSize maxTxSize
      where
        maxTxSize = toInteger (pp ^. ppMaxTxSizeL)
        zoneSize = totSizeZone z
    validateExUnitsTooBigUTxO ::
      PParams era ->
      [Tx era] ->
      Test (ConwayUtxoPredFailure era)
    validateExUnitsTooBigUTxO pp txs =
      failureUnless (pointWiseExUnits (<=) totalExUnits maxTxExUnits) $
        ExUnitsTooBigUTxO maxTxExUnits totalExUnits
      where
        maxTxExUnits = pp ^. ppMaxTxExUnitsL
        -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the zone:
        totalExUnits = Foldable.foldl' (<>) mempty $ fmap totExUnits txs
    -- TODO WG: This can probably be rolled in with the main check in the if expression
    chkExactlyLastInvalid :: [Tx era] -> Bool
    chkExactlyLastInvalid txs = case reverse txs of
      (l : txs') -> (l ^. isValidTxL == IsValid False) && all ((== IsValid True) . (^. isValidTxL)) txs'
      [] -> True

-- data ConwayUtxosEvent era
--   = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
--   | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
--   | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
--   | -- | The UTxOs consumed and created by a signal tx
--     TxUTxODiff
--       -- | UTxO consumed
--       (UTxO era)
--       -- | UTxO created
--       (UTxO era)
--   deriving (Generic)

conwayEvalScriptsTxInvalid ::
  forall era.
  ( EraRule "ZONE" era ~ ConwayZONE era
  , ConwayEraTxBody era
  , AlonzoEraTx era
  , Environment (EraRule "LEDGERS" era) ~ ConwayLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Embed (EraRule "LEDGERS" era) (ConwayZONE era)
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraRuleFailure "ZONE" era ~ ConwayZonePredFailure era
  , InjectRuleFailure "ZONE" AlonzoUtxosPredFailure era
  , InjectRuleFailure "ZONE" ConwayUtxosPredFailure era
  , InjectRuleFailure "ZONE" ConwayUtxoPredFailure era
  ) =>
  TransitionRule (ConwayZONE era)
conwayEvalScriptsTxInvalid =
  do
    TRC
      ( ConwayLedgersEnv _slotNo _ixRange pp _accountState
        , LedgerState us@(UTxOState utxo _ _ fees _ _ _) certState
        , txs :: Seq (Tx era)
        ) <-
      judgmentContext
    -- TODO WG: Is the list last first or last...last (Probably last last)
    let tx = last (Foldable.toList txs) -- TODO WG use safe head
        txBody = tx ^. bodyTxL

    -- TRC (UtxoEnv _ pp _, us@(UTxOState utxo _ _ fees _ _ _), tx) <- judgmentContext
    -- {- txb := txbody tx -}
    sysSt <- liftSTS $ asks systemStart
    ei <- liftSTS $ asks epochInfo

    () <- pure $! traceEvent invalidBegin ()

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
        DeltaCoin collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
    pure $!
      LedgerState
        us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
          { utxosUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
          {- fees + collateralFees -}
          , utxosFees = fees <> Coin collateralFees -- NEW to Babbage
          , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
          }
        certState

txInTxId :: TxIn era -> TxId era
txInTxId (TxIn x _) = x

-- get a set of TxIds containing all IDs of transaction in given list tb
getIDs :: EraTx era => [Tx era] -> Set (TxId (EraCrypto era))
getIDs = foldr (\tx ls -> ls `Set.union` Set.singleton (txIdTx tx)) mempty

mkEdges ::
  EraTx era =>
  Lens' (TxBody era) (Set (TxIn (EraCrypto era))) ->
  Tx era ->
  [Tx era] ->
  [(Tx era, Tx era)]
mkEdges l = go
  where
    go _ [] = []
    go tx (h : txs) =
      if txIdTx tx `elem` fmap (\(TxIn x _) -> x) (toList $ h ^. bodyTxL . l)
        then (tx, h) : go tx txs
        else go tx txs

-- make edges for a given transaction
mkIOEdges :: EraTx era => Tx era -> [Tx era] -> [(Tx era, Tx era)]
mkIOEdges = mkEdges inputsTxBodyL

-- make FR edges for a given transaction
mkFREdges :: (EraTx era, ConwayEraTxBody era) => Tx era -> [Tx era] -> [(Tx era, Tx era)]
mkFREdges = mkEdges fulfillsTxBodyL

-- make all edges for all transactions
mkAllEdges :: (EraTx era, ConwayEraTxBody era) => [Tx era] -> [Tx era] -> [(Tx era, Tx era)]
mkAllEdges [] _ = []
mkAllEdges (h : txs) ls = mkIOEdges h ls ++ mkFREdges h ls ++ mkAllEdges txs ls

-- -- for a given tx, and set of edges,
-- -- returns a list of transactions ls such that for each e in ls is such that e -> tx is a dependency
-- -- i.e. returns all ends of incoming edges
hasIncEdges :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era]
hasIncEdges _ [] = []
hasIncEdges tx ((e, tx') : edges) =
  if txIdTx tx == txIdTx tx'
    then e : hasIncEdges tx edges
    else hasIncEdges tx edges

-- -- filters a list of transactions such that only ones with no incoming edges remain
nodesWithNoIncomingEdge :: EraTx era => [Tx era] -> [(Tx era, Tx era)] -> [Tx era]
nodesWithNoIncomingEdge [] _ = []
nodesWithNoIncomingEdge (tx : txs) edges = case hasIncEdges tx edges of
  [] -> tx : nodesWithNoIncomingEdge txs edges
  _ -> nodesWithNoIncomingEdge txs edges

-- -- remove the first instance of a transaction in a list
removeTx :: EraTx era => Tx era -> [Tx era] -> [Tx era]
removeTx _ [] = []
removeTx tx (n : ne) =
  if txIdTx tx == txIdTx n
    then ne
    else n : removeTx tx ne

-- remove a transaction from a list if it has no incoming edges
ifNoEdgeRemove :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era] -> [Tx era]
ifNoEdgeRemove tx edges s = case hasIncEdges tx edges of
  [] -> removeTx tx s
  _ -> s

-- given tx1, for all tx such that (tx1 , tx) in edges,
--             remove (tx1 , tx) from the graph
--             if tx has no other incoming edges then
--               insert tx into S
updateRES :: EraTx era => Tx era -> [(Tx era, Tx era)] -> [Tx era] -> ([(Tx era, Tx era)], [Tx era])
updateRES _ [] s = ([], s)
updateRES tx1 ((tx, tx') : em) s =
  if txIdTx tx == txIdTx tx1
    then (fst (updateRES tx1 em (ifNoEdgeRemove tx em s)), ifNoEdgeRemove tx em s)
    else ((tx, tx') : fst (updateRES tx1 em s), s)

-- -- topologically sorts a tx list
-- -- arguments : tracking edges for agda termination check, remaining edges, remaining txs with no incoming edge (S), current sorted list (L)
-- -- returns nothing if there are remaining edges the graph, but S is empty
topSortTxs ::
  EraTx era => [(Tx era, Tx era)] -> [(Tx era, Tx era)] -> [Tx era] -> [Tx era] -> Maybe [Tx era]
topSortTxs _ [] _ srtd = Just srtd
topSortTxs [] _ _ _ = Nothing
topSortTxs _ _ [] _ = Nothing
topSortTxs ((tx1, _) : dges) (r : em) (tx : rls) srtd =
  uncurry (topSortTxs dges) updRES (srtd ++ [tx1])
  where
    updRES = updateRES tx1 (r : em) (removeTx tx1 (tx : rls))

instance
  ( Era era
  , STS (ConwayLEDGERS era)
  , PredicateFailure (EraRule "LEDGERS" era) ~ ShelleyLedgersPredFailure era
  ) =>
  Embed (ConwayLEDGERS era) (ConwayZONE era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInConwayEvent