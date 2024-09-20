{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Swaps (
  BabelSWAPS,
  BabelSwapsPredFailure (..),
  BabelSwapsEvent (..),
  BabelSwapsEnv (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
 )
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Babel.Era (
  BabelEra,
  BabelSWAPS,
 )
import Cardano.Ledger.Babel.Rules.Cert ()
import Cardano.Ledger.Babel.Rules.Certs ()
import Cardano.Ledger.Babel.Rules.Deleg ()
import Cardano.Ledger.Babel.Rules.Gov ()
import Cardano.Ledger.Babel.Rules.GovCert ()
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (
  BabelUtxoEnv (BabelUtxoEnv),
  BabelUtxosPredFailure,
  BatchData (..),
 )
import Cardano.Ledger.Babel.Rules.Utxow
import Cardano.Ledger.Babel.Tx (BabelEraTx (..))
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), TxIx, epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (Obligations, obligationCertState, sumObligation)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState (..),
  GovProcedures (..),
  Proposals,
  constitutionScriptL,
  pRootsL,
  proposalsGovStateL,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.Rules (
  CertEnv,
  CertsEnv (CertsEnv),
  ConwayCERTS,
  ConwayCertPredFailure,
  ConwayCertsEvent,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGOV,
  ConwayGovCertPredFailure,
  ConwayGovEvent,
  ConwayGovPredFailure,
  GovEnv (GovEnv),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.AdaPots (consumedTxBody, producedTxBody)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  CertState (..),
  DState (..),
  LedgerState (..),
  UTxOState (..),
  asTreasuryL,
  certVStateL,
  utxosDepositedL,
  utxosGovStateL,
  vsCommitteeStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Cardano.Ledger.Shelley.Rules.Reports (showTxCerts)
import Cardano.Ledger.Slot (SlotNo, epochInfoEpoch)
import Cardano.Ledger.UMap (UView (..), dRepMap)
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (EraUTxO (ScriptsNeeded))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Assertion (PostCondition),
  AssertionViolation (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  (?!),
 )
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic (..))
import Lens.Micro as L
import NoThunks.Class (NoThunks (..))

data BabelSwapsEnv era = BabelSwapsEnv
  { babelLedgerSlotNo :: !SlotNo
  , babelLedgerIx :: !TxIx
  , babelLedgerPp :: !(PParams era)
  , babelLedgerAccount :: !AccountState
  , babelLedgerRequireBatchObservers :: !(Set (ScriptHash (EraCrypto era)))
  , babelLedgerBatchData :: !(BatchData era)
  }
  deriving (Generic)

data BabelSwapsPredFailure era
  = BabelUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | BabelCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | BabelGovFailure (PredicateFailure (EraRule "GOV" era))
  | BabelWdrlNotDelegatedToDRep (Set (Credential 'Staking (EraCrypto era)))
  | BabelTreasuryValueMismatch
      -- | Actual
      Coin
      -- | Submitted in transaction
      Coin
  deriving (Generic)

type instance EraRuleFailure "SWAPS" (BabelEra c) = BabelSwapsPredFailure (BabelEra c)

type instance EraRuleEvent "SWAPS" (BabelEra c) = BabelSwapsEvent (BabelEra c)

instance InjectRuleFailure "SWAPS" BabelSwapsPredFailure (BabelEra c)

instance InjectRuleFailure "SWAPS" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" ShelleyUtxowPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" AlonzoUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" BabbageUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" ShelleyUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "SWAPS" ConwayCertsPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure

instance InjectRuleFailure "SWAPS" ConwayCertPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "SWAPS" ConwayDelegPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "SWAPS" ShelleyPoolPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "SWAPS" ConwayGovCertPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "SWAPS" ConwayGovPredFailure (BabelEra c) where
  injectFailure = BabelGovFailure . injectFailure

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  ) =>
  Eq (BabelSwapsPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  ) =>
  Show (BabelSwapsPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  ) =>
  NoThunks (BabelSwapsPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  ) =>
  NFData (BabelSwapsPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  EncCBOR (BabelSwapsPredFailure era)
  where
  encCBOR =
    encode . \case
      BabelUtxowFailure x -> Sum (BabelUtxowFailure @era) 1 !> To x
      BabelCertsFailure x -> Sum (BabelCertsFailure @era) 2 !> To x
      BabelGovFailure x -> Sum (BabelGovFailure @era) 3 !> To x
      BabelWdrlNotDelegatedToDRep x ->
        Sum (BabelWdrlNotDelegatedToDRep @era) 4 !> To x
      BabelTreasuryValueMismatch actual submitted ->
        Sum (BabelTreasuryValueMismatch @era) 5 !> To actual !> To submitted

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  DecCBOR (BabelSwapsPredFailure era)
  where
  decCBOR =
    decode $ Summands "BabelSwapsPredFailure" $ \case
      1 -> SumD BabelUtxowFailure <! From
      2 -> SumD BabelCertsFailure <! From
      3 -> SumD BabelGovFailure <! From
      4 -> SumD BabelWdrlNotDelegatedToDRep <! From
      5 -> SumD BabelTreasuryValueMismatch <! From <! From
      n -> Invalid n

data BabelSwapsEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  ) =>
  Eq (BabelSwapsEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
  ) =>
  NFData (BabelSwapsEvent era)

instance
  ( BabelEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (BabelSWAPS era)
  , Embed (EraRule "GOV" era) (BabelSWAPS era)
  , Embed (EraRule "CERTS" era) (BabelSWAPS era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ BabelUtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
  ) =>
  STS (BabelSWAPS era)
  where
  type State (BabelSWAPS era) = LedgerState era
  type Signal (BabelSWAPS era) = Tx era
  type Environment (BabelSWAPS era) = BabelSwapsEnv era
  type BaseM (BabelSWAPS era) = ShelleyBase
  type PredicateFailure (BabelSWAPS era) = BabelSwapsPredFailure era
  type Event (BabelSWAPS era) = BabelSwapsEvent era

  initialRules = []
  transitionRules = [ledgerTransition @BabelSWAPS]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = babelLedgerAssertions @era @BabelSWAPS

-- =======================================

{- CIP-0118#LEDGER-rule

Jump to CIP-0118#UTXOW-rule to continue... -}
ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ BabelSwapsEnv era
  , PredicateFailure (someLEDGER era) ~ BabelSwapsPredFailure era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "GOV" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ BabelUtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  , BabelEraTx era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (BabelSwapsEnv slot _txIx pp account bobs batchData, LedgerState utxoState certState, tx) <-
    judgmentContext

  let actualTreasuryValue = account ^. asTreasuryL
   in case tx ^. bodyTxL . currentTreasuryValueTxBodyL of
        SNothing -> pure ()
        SJust submittedTreasuryValue ->
          submittedTreasuryValue
            == actualTreasuryValue
              ?! BabelTreasuryValueMismatch actualTreasuryValue submittedTreasuryValue

  currentEpoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  let txBody = tx ^. bodyTxL
  let validPath = case (batchData, tx ^. isValidTxL) of
        (Batch _ (IsValid True), IsValid True) -> True
        (Batch _ (IsValid False), _) -> False
        (_, IsValid True) -> True
        (_, IsValid False) -> False

  (utxoState', certStateAfterCERTS) <-
    if validPath
      then do
        certStateAfterCERTS <-
          trans @(EraRule "CERTS" era) $
            TRC
              ( CertsEnv tx pp slot currentEpoch
              , certState
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )
        let wdrlAddrs = Map.keysSet . unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
            wdrlCreds = Set.map raCredential wdrlAddrs
            dUnified = dsUnified $ certDState certStateAfterCERTS
            delegatedAddrs = DRepUView dUnified

        -- TODO enable this check once delegation is fully implemented in cardano-api
        when False $ do
          all (`UMap.member` delegatedAddrs) wdrlCreds
            ?! BabelWdrlNotDelegatedToDRep (wdrlCreds Set.\\ Map.keysSet (dRepMap dUnified))

        -- Votes and proposals from signal tx
        let govProcedures =
              GovProcedures
                { gpVotingProcedures = txBody ^. votingProceduresTxBodyL
                , gpProposalProcedures = txBody ^. proposalProceduresTxBodyL
                }
        proposalsState <-
          trans @(EraRule "GOV" era) $
            TRC
              ( GovEnv
                  (txIdTxBody txBody)
                  currentEpoch
                  pp
                  (utxoState ^. utxosGovStateL . proposalsGovStateL . pRootsL . L.to toPrevGovActionIds)
                  (utxoState ^. utxosGovStateL . constitutionGovStateL . constitutionScriptL)
                  (certState ^. certVStateL . vsCommitteeStateL)
              , utxoState ^. utxosGovStateL . proposalsGovStateL
              , govProcedures
              )
        let utxoState' =
              utxoState
                & utxosGovStateL
                . proposalsGovStateL
                .~ proposalsState
        pure
          (utxoState', certStateAfterCERTS)
      else pure (utxoState, certState)

  utxoState'' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        -- Pass to UTXOW the unmodified CertState in its Environment,
        -- so it can process refunds of deposits for deregistering
        -- stake credentials and DReps. The modified CertState
        -- (certStateAfterCERTS) has these already removed from its
        -- UMap.
        ( BabelUtxoEnv @era slot pp certState bobs batchData
        , utxoState'
        , tx
        )
  pure (LedgerState utxoState'' certStateAfterCERTS)

instance
  ( Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , BaseM (BabelUTXOW era) ~ ShelleyBase
  , AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXOW" era) (BabelSWAPS era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Environment (EraRule "UTXOW" era) ~ BabelUtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabelUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (BabelUTXOW era)
  , PredicateFailure (BabelUTXOW era) ~ BabelUtxowPredFailure era
  , Event (BabelUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabelUTXOW era) (BabelSWAPS era)
  where
  wrapFailed = BabelUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , ConwayEraGov era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , State (EraRule "CERT" era) ~ CertState era
  , Environment (EraRule "CERT" era) ~ CertEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , PredicateFailure (EraRule "CERTS" era) ~ ConwayCertsPredFailure era
  , Event (EraRule "CERTS" era) ~ ConwayCertsEvent era
  , EraRule "CERTS" era ~ ConwayCERTS era
  ) =>
  Embed (ConwayCERTS era) (BabelSWAPS era)
  where
  wrapFailed = BabelCertsFailure
  wrapEvent = CertsEvent

instance
  ( ConwayEraPParams era
  , BaseM (BabelSWAPS era) ~ ShelleyBase
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  ) =>
  Embed (ConwayGOV era) (BabelSWAPS era)
  where
  wrapFailed = BabelGovFailure
  wrapEvent = GovEvent

-- Helpers

renderDepositEqualsObligationViolation ::
  ( EraTx era
  , EraGov era
  , Environment t ~ BabelSwapsEnv era
  , Signal t ~ Tx era
  , State t ~ LedgerState era
  ) =>
  AssertionViolation t ->
  String
renderDepositEqualsObligationViolation
  AssertionViolation
    { avSTS
    , avMsg
    , avCtx = TRC (BabelSwapsEnv slot _ pp _ _bobs _batchData, _, tx)
    , avState
    } =
    case avState of
      Nothing -> "\nAssertionViolation " ++ avSTS ++ " " ++ avMsg ++ " (avState is Nothing)."
      Just lstate ->
        let certstate = lsCertState lstate
            utxoSt = lsUTxOState lstate
            utxo = utxosUtxo utxoSt
            txb = tx ^. bodyTxL
            pot = utxoSt ^. utxosDepositedL
         in "\n\nAssertionViolation ("
              <> avSTS
              <> ")\n\n  "
              <> avMsg
              <> "\n\nCERTS\n"
              <> showTxCerts txb
              <> "\n(slot,keyDeposit,poolDeposit) "
              <> show (slot, pp ^. ppKeyDepositL, pp ^. ppPoolDepositL)
              <> "\nThe Pot (utxosDeposited) = "
              <> show pot
              <> "\n"
              <> show (allObligations certstate (utxosGovState utxoSt))
              <> "\nConsumed = "
              <> show (consumedTxBody txb pp certstate utxo)
              <> "\nProduced = "
              <> show (producedTxBody txb pp certstate)

babelLedgerAssertions ::
  ( EraGov era
  , State (rule era) ~ LedgerState era
  ) =>
  [Assertion (rule era)]
babelLedgerAssertions =
  [ PostCondition
      "Deposit pot must equal obligation (LEDGER)"
      ( \(TRC (_, _, _))
         (LedgerState utxoSt dpstate) -> potEqualsObligation dpstate utxoSt
      )
  ]

potEqualsObligation ::
  EraGov era =>
  CertState era ->
  UTxOState era ->
  Bool
potEqualsObligation certState utxoSt = obligations == pot
  where
    obligations = totalObligation certState (utxoSt ^. utxosGovStateL)
    pot = utxoSt ^. utxosDepositedL

allObligations :: EraGov era => CertState era -> GovState era -> Obligations
allObligations certState govState =
  obligationCertState certState <> obligationGovState govState

totalObligation :: EraGov era => CertState era -> GovState era -> Coin
totalObligation certState govState = sumObligation (allObligations certState govState)
