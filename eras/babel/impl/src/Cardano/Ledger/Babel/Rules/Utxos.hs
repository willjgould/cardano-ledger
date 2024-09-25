{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Utxos (
  BabelUTXOS,
  BabelUtxosPredFailure (..),
  BabelUtxosEvent (..),
  BatchData (..),
  BabelUtxoEnv (..),
  isTop,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (..),
  evalPlutusScripts,
  lookupPlutusScript,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosEvent,
  AlonzoUtxosPredFailure,
  TagMismatchDescription (..),
  invalidBegin,
  invalidEnd,
  validBegin,
  validEnd,
  when2Phase,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (lookupRedeemer)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded (..),
  getMintingScriptsNeeded,
  getRewardingScriptsNeeded,
  getSpendingScriptsNeeded,
  zipAsIxItem,
 )
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Rules (
  expectScriptsToPass,
 )
import Cardano.Ledger.Babel.Scripts

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra, BabelUTXOS)
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody (..))
import Cardano.Ledger.Babel.TxInfo ()
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Sized (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (certsTotalDepositsTxBody, certsTotalRefundsTxBody)
import Cardano.Ledger.Coin (Coin (Coin), DeltaCoin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayGovState (..),
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Credential (Credential (..), credScriptHash)
import Cardano.Ledger.Plutus (
  PlutusWithContext (..),
  ScriptFailure (..),
  costModelsValid,
  getPlutusData,
 )
import Cardano.Ledger.Plutus.Evaluate (PlutusDatums (..), ScriptResult (..))
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  UTxOState (..),
  updateStakeDistribution,
  utxosDonationL,
 )
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Ledger.TxIn (TxId)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (UTxO, unUTxO), getScriptHash)
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (guard, when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence (mapWithIndex)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks)

data BatchData era -- or some better name
  = OldTransaction
  | NormalTransaction
  | Batch (TxId (EraCrypto era)) IsValid -- meaning batch valid
  deriving (Eq)

isTop :: EraTx era => BatchData era -> Tx era -> Bool
isTop (Batch tid _) tx = tid == txIdTx tx
isTop _ _ = False

data BabelUtxoEnv era = BabelUtxoEnv
  { bueSlot :: SlotNo
  , buePParams :: PParams era
  , bueCertState :: CertState era
  , bueRequiredBatchObservers :: Set.Set (ScriptHash (EraCrypto era))
  , bueBatchData :: BatchData era
  }
  deriving (Generic)

data BabelUtxosPredFailure era
  = -- | The 'isValid' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.). The Text tries to explain why it failed.
    ValidationTagMismatch IsValid TagMismatchDescription
  | -- | We could not find all the necessary inputs for a Plutus Script.
    -- Previous PredicateFailure tests should make this impossible, but the
    -- consequences of not detecting this means scripts get dropped, so things
    -- might validate that shouldn't. So we double check in the function
    -- collectTwoPhaseScriptInputs, it should find data for every Script.
    CollectErrors [CollectError era]
  deriving
    (Generic)

data BabelUtxosEvent era
  = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (BabelUtxosEvent era)

instance (Era era, NFData (TxOut era)) => NFData (BabelUtxosEvent era)

type instance EraRuleFailure "UTXOS" (BabelEra c) = BabelUtxosPredFailure (BabelEra c)

type instance EraRuleEvent "UTXOS" (BabelEra c) = BabelUtxosEvent (BabelEra c)

instance InjectRuleFailure "UTXOS" BabelUtxosPredFailure (BabelEra c)

instance InjectRuleEvent "UTXOS" BabelUtxosEvent (BabelEra c)

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = alonzoToBabelUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent (BabelEra c) where
  injectEvent = alonzoToBabelUtxosEvent

alonzoToBabelUtxosPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosPredFailure era ->
  BabelUtxosPredFailure era
alonzoToBabelUtxosPredFailure = \case
  Alonzo.ValidationTagMismatch t x -> ValidationTagMismatch t x
  Alonzo.CollectErrors x -> CollectErrors x
  Alonzo.UpdateFailure x -> absurdEraRule @"PPUP" @era x

alonzoToBabelUtxosEvent ::
  forall era.
  EraRuleEvent "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosEvent era ->
  BabelUtxosEvent era
alonzoToBabelUtxosEvent = \case
  Alonzo.AlonzoPpupToUtxosEvent x -> absurdEraRule @"PPUP" @era x
  Alonzo.TotalDeposits h c -> TotalDeposits h c
  Alonzo.SuccessfulPlutusScriptsEvent l -> SuccessfulPlutusScriptsEvent l
  Alonzo.FailedPlutusScriptsEvent l -> FailedPlutusScriptsEvent l
  Alonzo.TxUTxODiff x y -> TxUTxODiff x y

instance
  ( EraTxCert era
  , EncCBOR (ContextError era)
  , ConwayEraScript era
  ) =>
  EncCBOR (BabelUtxosPredFailure era)
  where
  encCBOR =
    encode . \case
      ValidationTagMismatch v descr -> Sum ValidationTagMismatch 0 !> To v !> To descr
      CollectErrors cs -> Sum (CollectErrors @era) 1 !> To cs

instance
  ( EraTxCert era
  , DecCBOR (ContextError era)
  , ConwayEraScript era
  ) =>
  DecCBOR (BabelUtxosPredFailure era)
  where
  decCBOR = decode (Summands "BabelUtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec n = Invalid n

deriving stock instance
  ( Show (TxCert era)
  , Show (ContextError era)
  , Show (UTxOState era)
  , ConwayEraScript era
  ) =>
  Show (BabelUtxosPredFailure era)

deriving stock instance
  ( Eq (TxCert era)
  , Eq (ContextError era)
  , Eq (UTxOState era)
  , ConwayEraScript era
  ) =>
  Eq (BabelUtxosPredFailure era)

instance
  ( NoThunks (TxCert era)
  , NoThunks (ContextError era)
  , NoThunks (UTxOState era)
  , ConwayEraScript era
  ) =>
  NoThunks (BabelUtxosPredFailure era)

instance
  ( NFData (TxCert era)
  , NFData (ContextError era)
  , NFData (UTxOState era)
  , ConwayEraScript era
  ) =>
  NFData (BabelUtxosPredFailure era)

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (BabelUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ BabelUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  , BabelEraTxBody era
  , BabelEraScript era
  ) =>
  STS (BabelUTXOS era)
  where
  type BaseM (BabelUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (BabelUTXOS era) = BabelUtxoEnv era
  type State (BabelUTXOS era) = UTxOState era
  type Signal (BabelUTXOS era) = Tx era
  type PredicateFailure (BabelUTXOS era) = BabelUtxosPredFailure era
  type Event (BabelUTXOS era) = BabelUtxosEvent era

  transitionRules = [utxosTransition]

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ BabelUtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  , Event (EraRule "UTXOS" era) ~ BabelUtxosEvent era
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  , BabelEraTxBody era
  , BabelEraScript era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
utxosTransition =
  judgmentContext >>= \(TRC (BabelUtxoEnv _ _ _ _ batchData, _, tx)) -> do
    if validPath batchData tx
      then babelEvalScriptsTxValid
      else babelEvalScriptsTxInvalid

babelEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Environment (EraRule "UTXOS" era) ~ BabelUtxoEnv era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" BabelUtxosEvent era
  , BabelEraTxBody era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
babelEvalScriptsTxValid = do
  TRC (BabelUtxoEnv _ pp certState _bobs _, utxos@(UTxOState utxo _ _ govState _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL

  () <- pure $! traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! traceEvent validEnd ()

  utxos' <-
    updateUTxOState
      pp
      utxos
      txBody
      certState
      govState
      (tellEvent . injectEvent . TotalDeposits (hashAnnotated txBody))
      (\a b -> tellEvent . injectEvent $ TxUTxODiff a b)
  pure $!
    utxos' & utxosDonationL <>~ txBody ^. treasuryDonationTxBodyL

-- | This monadic action captures the final stages of the UTXO(S) rule. In particular it
-- applies all of the UTxO related aditions and removals, gathers all of the fees into the
-- fee pot `utxosFees` and updates the `utxosDeposited` field. Continuation supplied will
-- be called on the @deposit - refund@ change, which is normally used to emit the
-- `TotalDeposits` event.
updateUTxOState ::
  (Monad m, BabelEraTxBody era) =>
  PParams era ->
  UTxOState era ->
  TxBody era ->
  CertState era ->
  GovState era ->
  (Coin -> m ()) ->
  (UTxO era -> UTxO era -> m ()) ->
  m (UTxOState era)
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent = do
  let UTxOState
        { utxosUtxo
        , utxosDeposited
        , utxosFees
        , utxosStakeDistr
        , utxosDonation
        } = utxos
      UTxO utxo = utxosUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo ((txBody ^. inputsTxBodyL) `Set.union` (txBody ^. corInputsTxBodyL)) -- TODO WG Check this is correct
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      {- utxoDel  = txins txb ◁ utxo -}
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      deletedUTxO = UTxO utxoDel
      newIncStakeDistro = updateStakeDistribution pp utxosStakeDistr deletedUTxO utxoAdd
      totalRefunds = certsTotalRefundsTxBody pp certState txBody
      totalDeposits = certsTotalDepositsTxBody pp certState txBody
      depositChange = totalDeposits <-> totalRefunds
  depositChangeEvent depositChange
  txUtxODiffEvent deletedUTxO utxoAdd
  pure $!
    UTxOState
      { utxosUtxo = UTxO newUTxO
      , utxosDeposited = utxosDeposited <> depositChange
      , utxosFees = utxosFees <> txBody ^. feeTxBodyL
      , utxosGovState = govState
      , utxosStakeDistr = newIncStakeDistro
      , utxosDonation = utxosDonation
      }

babelEvalScriptsTxInvalid ::
  forall era.
  ( AlonzoEraTx era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ BabelUtxoEnv era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , Event (EraRule "UTXOS" era) ~ BabelUtxosEvent era
  , PredicateFailure (EraRule "UTXOS" era) ~ BabelUtxosPredFailure era
  , ConwayEraTxBody era
  , BabelEraScript era
  , BabelEraTxBody era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
babelEvalScriptsTxInvalid = do
  TRC (BabelUtxoEnv _ pp _ _bobs batchData, us@(UTxOState utxo _ fees _ _ _), tx) <- judgmentContext
  {- txb := txbody tx -}
  let txBody = tx ^. bodyTxL
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent invalidBegin ()

  case collectPlutusScriptsWithContext batchData ei sysSt pp tx utxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts tx sLst of
          Passes _ ->
            when (IsValid False == tx ^. isValidTxL) $
              failBecause $
                ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_ (tellEvent . SuccessfulPlutusScriptsEvent @era) (nonEmpty ps)
            tellEvent (FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs))
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
  {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
  let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
      UTxO collouts = collOuts txBody
      DeltaCoin collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
  if isTop batchData tx
    then
      pure $!
        us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
          { utxosUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
          {- fees + collateralFees -}
          , utxosFees = fees <> Coin collateralFees -- NEW to Babbage
          , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
          }
    else pure us

-- To Babel Fees implementers: This function is NOT in the right place.
-- Given more time, I'd do something with the EraUTxO class.
collectPlutusScriptsWithContext ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ConwayEraTxBody era
  , BabelEraScript era
  , BabelEraTxBody era
  ) =>
  BatchData era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [PlutusWithContext (EraCrypto era)]
collectPlutusScriptsWithContext batchData epochInfo sysStart pp tx utxo =
  -- TODO: remove this whole complicated check when we get into Conway. It is much simpler
  -- to fail on a CostModel lookup in the `apply` function (already implemented).
  {- languages tx utxo ⊆ dom(costmdls pp) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'.
  let missingCostModels = Set.filter (`Map.notMember` costModels) usedLanguages
   in case guard (protVerMajor < natVersion @9) >> Set.lookupMin missingCostModels of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            apply
            (map getScriptWithRedeemer neededPlutusScripts)
            (Right [])
  where
    -- Check on a protocol version to preserve failure mode (a single NoCostModel failure
    -- for languages with missing cost models) until we are in Conway era. After we hard
    -- fork into Conway it will be safe to remove this check together with the
    -- `missingCostModels` lookup
    --
    -- We also need to pass major protocol version to the script for script evaluation
    protVerMajor = pvMajor (pp ^. ppProtocolVersionL)

    -- TODO WG: You really need a unit test for this.
    costModels = costModelsValid $ pp ^. ppCostModelsL

    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = hackyGetScriptsNeeded batchData utxo (tx ^. bodyTxL)
    neededPlutusScripts =
      mapMaybe (\(sp, sh) -> (,) (sh, sp) <$> lookupPlutusScript scriptsProvided sh) scriptsNeeded
    usedLanguages = Set.fromList $ map (plutusScriptLanguage . snd) neededPlutusScripts

    getScriptWithRedeemer ((plutusScriptHash, plutusPurpose), plutusScript) =
      let redeemerIndex = hoistPlutusPurpose toAsIx plutusPurpose
       in case lookupRedeemer redeemerIndex $ tx ^. witsTxL . rdmrsTxWitsL of
            Just (d, exUnits) -> Right (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash)
            Nothing -> Left (NoRedeemer (hoistPlutusPurpose toAsItem plutusPurpose))
    apply (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash) = do
      let lang = plutusScriptLanguage plutusScript
      costModel <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      case mkPlutusScriptContext plutusScript plutusPurpose pp epochInfo sysStart utxo tx of
        Right scriptContext ->
          let spendingDatum = getSpendingDatum utxo tx $ hoistPlutusPurpose toAsItem plutusPurpose
              datums = maybe id (:) spendingDatum [d, scriptContext]
           in Right $
                withPlutusScript plutusScript $ \plutus ->
                  PlutusWithContext
                    { pwcProtocolVersion = protVerMajor
                    , pwcScript = Left plutus
                    , pwcScriptHash = plutusScriptHash
                    , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                    , pwcExUnits = exUnits
                    , pwcCostModel = costModel
                    }
        Left te -> Left $ BadTranslation te

-- isOld :: BatchData era -> Bool
-- isOld OldTransaction = True
-- isOld _ = False

-- TODO: check this
-- allowedLanguages ::
--   (EraTx era, EraUTxO era) => BatchData era -> Tx era -> UTxO era -> Set.Set Language
-- allowedLanguages bd tx (UTxO utxo)
--   | any (isBootstrapAddr . fst) os = Set.empty
--   | usesV3Features txb = Set.fromList [PlutusV3]
--   | any hasInlineDatum os = Set.fromList [PlutusV2, PlutusV3]
--   | isOld bd = Set.fromList [PlutusV2, PlutusV3, PlutusV4]
--   | otherwise = Set.fromList [PlutusV1, PlutusV2, PlutusV3, PlutusV4]
--   where
--     txb = tx ^. bodyTxL
--     os = Set.union (range $ outs txb) (range $ utxo `restrictKeys` (txins `Set.union` refInputs))

--     outs = tx ^. bodyTxL . txOutsTxBodyL
--     txins = tx ^. bodyTxL . inputsTxBodyL
--     refInputs = tx ^. bodyTxL . referenceInputsTxBodyL

-- collectPhaseTwoScriptInputs' : BatchData → PParams → Tx → UTxO → (ScriptPurpose × ScriptHash)
--   → Maybe (Script × List Data × ExUnits × CostModel)
-- collectPhaseTwoScriptInputs' bd pp tx utxo (sp , sh)
--   with lookupScriptHash sh tx utxo
-- ... | nothing = nothing
-- ... | just s
--   with isInj₂ s | indexedRdmrs tx sp
-- ... | just p2s | just (rdmr , eu)
--     = just (s ,
--         ( (getDatum tx utxo sp ++ rdmr ∷ valContext (txInfo (language p2s) sp pp utxo tx) sp ∷ [])
--         , eu
--         , PParams.costmdls pp)
--       )
-- ... | x | y = nothing

-- collectPhaseTwoScriptInputs : BatchData → PParams → Tx → UTxO
--   → List (Script × List Data × ExUnits × CostModel)
-- collectPhaseTwoScriptInputs bd pp tx utxo
--   = setToList
--   $ mapPartial (collectPhaseTwoScriptInputs' bd pp tx utxo)
--   $ scriptsNeeded bd utxo tx

-- | Merge two lists (the first of which may have failures, i.e. (Left _)), collect all the failures
--   but if there are none, use 'f' to construct a success.
merge :: forall t b a. (t -> Either a b) -> [Either a t] -> Either [a] [b] -> Either [a] [b]
merge _f [] answer = answer
merge f (x : xs) zs = merge f xs (gg x zs)
  where
    gg :: Either a t -> Either [a] [b] -> Either [a] [b]
    gg (Right t) (Right cs) =
      case f t of
        Right c -> Right $ c : cs
        Left e -> Left [e]
    gg (Left a) (Right _) = Left [a]
    gg (Right _) (Left cs) = Left cs
    gg (Left a) (Left cs) = Left (a : cs)

-- TODO WG: I think you need to use this in UTXOW as well.
hackyGetScriptsNeeded ::
  (ConwayEraTxBody era, BabelEraScript era, BabelEraTxBody era) =>
  BatchData era ->
  UTxO era ->
  TxBody era ->
  AlonzoScriptsNeeded era
hackyGetScriptsNeeded batchData utxo txBody =
  getSpendingScriptsNeeded utxo txBody
    <> getRewardingScriptsNeeded txBody
    <> certifyingScriptsNeeded
    <> getMintingScriptsNeeded txBody
    <> votingScriptsNeeded
    <> proposingScriptsNeeded
    <> getBatchObserverScriptsNeeded batchData txBody
    <> getSpendOutScriptsNeeded
      ( Map.fromList . toList $
          mapWithIndex
            (\i txOut -> (TxIx $ fromIntegral i, txOut))
            (sizedValue <$> (Seq.fromList . toList) (txBody ^. spendOutsTxBodyL)) -- mapPartial spendOutScripts (proj₁ spendOuts)
      )
  where
    certifyingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. certsTxBodyL) $
            \asIxItem@(AsIxItem _ txCert) ->
              (CertifyingPurpose asIxItem,) <$> getScriptWitnessTxCert txCert

    votingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (Map.keys (unVotingProcedures (txBody ^. votingProceduresTxBodyL))) $
            \asIxItem@(AsIxItem _ voter) ->
              (VotingPurpose asIxItem,) <$> getVoterScriptHash voter
      where
        getVoterScriptHash = \case
          CommitteeVoter cred -> credScriptHash cred
          DRepVoter cred -> credScriptHash cred
          StakePoolVoter _ -> Nothing

    proposingScriptsNeeded =
      AlonzoScriptsNeeded $
        catMaybes $
          zipAsIxItem (txBody ^. proposalProceduresTxBodyL) $
            \asIxItem@(AsIxItem _ proposal) ->
              (ProposingPurpose asIxItem,) <$> getProposalScriptHash proposal
      where
        getProposalScriptHash ProposalProcedure {pProcGovAction} =
          case pProcGovAction of
            ParameterChange _ _ (SJust govPolicyHash) -> Just govPolicyHash
            TreasuryWithdrawals _ (SJust govPolicyHash) -> Just govPolicyHash
            _ -> Nothing

getBatchObserverScriptsNeeded ::
  (BabelEraScript era, BabelEraTxBody era) =>
  BatchData era ->
  TxBody era ->
  AlonzoScriptsNeeded era
getBatchObserverScriptsNeeded batchData txBody =
  AlonzoScriptsNeeded $
    catMaybes $
      zipAsIxItem (getBOs batchData txBody) $
        \asIxItem@(AsIxItem _ batchObsHash) -> do
          pure (BatchObsPurpose asIxItem, batchObsHash)
{-# INLINEABLE getBatchObserverScriptsNeeded #-}

getSpendOutScriptsNeeded ::
  (EraTxBody era, BabelEraScript era) =>
  Map.Map TxIx (TxOut era) ->
  AlonzoScriptsNeeded era
getSpendOutScriptsNeeded spendOuts =
  AlonzoScriptsNeeded $
    catMaybes $
      zipAsIxItem (Map.keys spendOuts) $ -- (catMaybes $ fmap scriptOutWithHashNoIn $ toList spendOuts) $
        \asIxItem@(AsIxItem _ spendOut) -> do
          (SpendOutPurpose asIxItem,) <$> scriptOutWithHashNoIn (spendOuts Map.! spendOut)
{-# INLINEABLE getSpendOutScriptsNeeded #-}

-- Agda for reference:
-- data isScript : Credential → Type where
--   SHisScript : (sh : ScriptHash) → isScript (ScriptObj sh)
-- isScriptAddr     = isScript ∘ payCred

isScriptAddr :: Addr c -> Bool
isScriptAddr (Addr _ (ScriptHashObj _) _) = True
isScriptAddr _ = False

scriptOutWithHashNoIn :: EraTxOut era => TxOut era -> Maybe (ScriptHash (EraCrypto era))
scriptOutWithHashNoIn (view addrTxOutL -> addr) =
  if isScriptAddr addr
    then getScriptHash addr
    else Nothing

-- spendOutScripts :: (TxIx, TxOut era) -> Maybe (ScriptPurpose, ScriptHash (EraCrypto era))
-- spendOutScripts (x, o) = case (x, scriptOutWithHashNoIn o) of
--   (x, Just jo) -> Just (SpendOut x, jo)
--   (x, Nothing) -> Nothing

-- only return batch observer scripts if this is a full batch, and the transaction is top-level
getBOs ::
  BabelEraTxBody era =>
  BatchData era ->
  TxBody era ->
  Set (ScriptHash (EraCrypto era))
getBOs (Batch tid _) body =
  if tid == txIdTxBody body
    then body ^. requireBatchObserversTxBodyL
    else mempty
getBOs _ _ = mempty

validPath :: AlonzoEraTx era => BatchData era -> Tx era -> Bool
validPath (Batch _ (IsValid b)) _ = b
validPath NormalTransaction tx = IsValid True == (tx ^. isValidTxL)
validPath OldTransaction tx = IsValid True == (tx ^. isValidTxL)