{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Utxow (
  alonzoToBabelUtxowPredFailure,
  babbageToBabelUtxowPredFailure,
  BabelUTXOW,
  BabelUtxowPredFailure (..),
  babelWitsVKeyNeeded,
  shelleyToBabelUtxowPredFailure,
)
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure,
  hasExactSetOfRedeemers,
  ppViewHashesMatch,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoUtxowPredFailure (..))
import Cardano.Ledger.Alonzo.TxWits (unTxDats)
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
  babbageMissingScripts,
  validateFailedBabbageScripts,
  validateScriptsWellFormed,
 )
import qualified Cardano.Ledger.Babbage.Rules as Babbage (
  BabbageUtxowPredFailure (..),
 )
import Cardano.Ledger.Babbage.UTxO
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra, BabelUTXO, BabelUTXOW)
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (
  BabelUtxoEnv (BabelUtxoEnv),
  BabelUtxosPredFailure,
  BatchData (..),
  isTop,
 )
import Cardano.Ledger.Babel.Tx
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody (..), ConwayEraTxBody)
import Cardano.Ledger.Babel.UTxO (getBabelWitsVKeyNeeded)
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), sizedValue)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core (ConwayEraScript)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  VKey,
 )
import Cardano.Ledger.Plutus (Datum (..))
import Cardano.Ledger.Rules.ValidationMode (Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  validateNeededWitnesses,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (
  ShelleyUtxowPredFailure (..),
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley hiding (UtxoFailure)
import Cardano.Ledger.Shelley.Tx (witsFromTxWitnesses)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Control.DeepSeq (NFData)
import Control.SetAlgebra hiding (Embed)
import Control.State.Transition.Extended
import Data.Foldable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Validation

babelWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
babelWitsVKeyNeeded = getBabelWitsVKeyNeeded
{-# DEPRECATED babelWitsVKeyNeeded "In favor of `getBabelWitsVKeyNeeded` or `getWitsVKeyNeeded`" #-}

-- ================================

-- | Predicate failure type for the Babel Era
data BabelUtxowPredFailure era
  = UtxoFailure (PredicateFailure (EraRule "UTXO" era))
  | InvalidWitnessesUTXOW
      ![VKey 'Witness (EraCrypto era)]
  | -- | witnesses which failed in verifiedWits function
    MissingVKeyWitnessesUTXOW
      -- | witnesses which were needed and not supplied
      !(Set (KeyHash 'Witness (EraCrypto era)))
  | -- | missing scripts
    MissingScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | failed scripts
    ScriptWitnessNotValidatingUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | -- | hash of the full metadata
    MissingTxBodyMetadataHash
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | hash of the metadata included in the transaction body
    MissingTxMetadata
      !(AuxiliaryDataHash (EraCrypto era))
  | ConflictingMetadataHash
      -- | hash of the metadata included in the transaction body
      !(AuxiliaryDataHash (EraCrypto era))
      -- | expected hash of the full metadata
      !(AuxiliaryDataHash (EraCrypto era))
  | -- | Contains out of range values (string`s too long)
    InvalidMetadata
  | -- | extraneous scripts
    ExtraneousScriptWitnessesUTXOW
      !(Set (ScriptHash (EraCrypto era)))
  | MissingRedeemers
      ![(PlutusPurpose AsItem era, ScriptHash (EraCrypto era))]
  | MissingRequiredDatums
      -- | Set of missing data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of received data hashes
      !(Set (DataHash (EraCrypto era)))
  | NotAllowedSupplementalDatums
      -- | Set of unallowed data hashes
      !(Set (DataHash (EraCrypto era)))
      -- | Set of acceptable supplemental data hashes
      !(Set (DataHash (EraCrypto era)))
  | PPViewHashesDontMatch
      -- | The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
      -- | Computed from the current Protocol Parameters
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers ![PlutusPurpose AsIx era]
  | -- | Embed UTXO rule failures
    MalformedScriptWitnesses
      !(Set (ScriptHash (EraCrypto era)))
  | -- | the set of malformed script witnesses
    MalformedReferenceScripts
      !(Set (ScriptHash (EraCrypto era)))
  | SubsInSubsUtxowFailure -- Subtransactions cannot contain subtransactions
  | BatchObserversFailure
  deriving (Generic)

type instance EraRuleFailure "UTXOW" (BabelEra c) = BabelUtxowPredFailure (BabelEra c)

type instance EraRuleEvent "UTXOW" (BabelEra c) = AlonzoUtxowEvent (BabelEra c)

instance InjectRuleFailure "UTXOW" BabelUtxowPredFailure (BabelEra c)

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure (BabelEra c) where
  injectFailure = babbageToBabelUtxowPredFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (BabelEra c) where
  injectFailure = alonzoToBabelUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (BabelEra c) where
  injectFailure = shelleyToBabelUtxowPredFailure

instance InjectRuleFailure "UTXOW" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (BabelEra c) where
  injectFailure = UtxoFailure . injectFailure

deriving instance
  ( ConwayEraScript era
  , Show (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Show (BabelUtxowPredFailure era)

deriving instance
  ( ConwayEraScript era
  , Eq (PredicateFailure (EraRule "UTXO" era))
  ) =>
  Eq (BabelUtxowPredFailure era)

deriving via
  InspectHeapNamed "BabelUtxowPred" (BabelUtxowPredFailure era)
  instance
    NoThunks (BabelUtxowPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (PredicateFailure (EraRule "UTXO" era))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (BabelUtxowPredFailure era)

--------------------------------------------------------------------------------
-- BabelUTXOW STS
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ BabelUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabelUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (BabelUTXOW era)
  , Environment (EraRule "UTXO" era) ~ BabelUtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  , BabelEraTxBody era
  ) =>
  STS (BabelUTXOW era)
  where
  type State (BabelUTXOW era) = UTxOState era
  type Signal (BabelUTXOW era) = Tx era
  type Environment (BabelUTXOW era) = BabelUtxoEnv era
  type BaseM (BabelUTXOW era) = ShelleyBase
  type PredicateFailure (BabelUTXOW era) = BabelUtxowPredFailure era
  type Event (BabelUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babelUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (BabelUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ BabelUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (BabelUTXOW era) ~ ShelleyBase
  , PredicateFailure (BabelUTXOW era) ~ BabelUtxowPredFailure era
  , Event (BabelUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabelUTXO era) (BabelUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . Shelley.UtxoEvent

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( ConwayEraScript era
  , EncCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  EncCBOR (BabelUtxowPredFailure era)
  where
  encCBOR =
    encode . \case
      UtxoFailure x -> Sum UtxoFailure 0 !> To x
      InvalidWitnessesUTXOW xs -> Sum InvalidWitnessesUTXOW 1 !> To xs
      MissingVKeyWitnessesUTXOW xs -> Sum MissingVKeyWitnessesUTXOW 2 !> To xs
      MissingScriptWitnessesUTXOW xs -> Sum MissingScriptWitnessesUTXOW 3 !> To xs
      ScriptWitnessNotValidatingUTXOW xs -> Sum ScriptWitnessNotValidatingUTXOW 4 !> To xs
      MissingTxBodyMetadataHash xs -> Sum MissingTxBodyMetadataHash 5 !> To xs
      MissingTxMetadata xs -> Sum MissingTxMetadata 6 !> To xs
      ConflictingMetadataHash a b -> Sum ConflictingMetadataHash 7 !> To a !> To b
      InvalidMetadata -> Sum InvalidMetadata 8
      ExtraneousScriptWitnessesUTXOW xs -> Sum ExtraneousScriptWitnessesUTXOW 9 !> To xs
      MissingRedeemers x -> Sum MissingRedeemers 10 !> To x
      MissingRequiredDatums x y -> Sum MissingRequiredDatums 11 !> To x !> To y
      NotAllowedSupplementalDatums x y -> Sum NotAllowedSupplementalDatums 12 !> To x !> To y
      PPViewHashesDontMatch x y -> Sum PPViewHashesDontMatch 13 !> To x !> To y
      UnspendableUTxONoDatumHash x -> Sum UnspendableUTxONoDatumHash 14 !> To x
      ExtraRedeemers x -> Sum ExtraRedeemers 15 !> To x
      MalformedScriptWitnesses x -> Sum MalformedScriptWitnesses 16 !> To x
      MalformedReferenceScripts x -> Sum MalformedReferenceScripts 17 !> To x
      SubsInSubsUtxowFailure -> Sum SubsInSubsUtxowFailure 18
      BatchObserversFailure -> Sum BatchObserversFailure 19

instance
  ( ConwayEraScript era
  , DecCBOR (PredicateFailure (EraRule "UTXO" era))
  ) =>
  DecCBOR (BabelUtxowPredFailure era)
  where
  decCBOR = decode . Summands "BabelUtxowPred" $ \case
    0 -> SumD UtxoFailure <! From
    1 -> SumD InvalidWitnessesUTXOW <! From
    2 -> SumD MissingVKeyWitnessesUTXOW <! From
    3 -> SumD MissingScriptWitnessesUTXOW <! From
    4 -> SumD ScriptWitnessNotValidatingUTXOW <! From
    5 -> SumD MissingTxBodyMetadataHash <! From
    6 -> SumD MissingTxMetadata <! From
    7 -> SumD ConflictingMetadataHash <! From <! From
    8 -> SumD InvalidMetadata
    9 -> SumD ExtraneousScriptWitnessesUTXOW <! From
    10 -> SumD MissingRedeemers <! From
    11 -> SumD MissingRequiredDatums <! From <! From
    12 -> SumD NotAllowedSupplementalDatums <! From <! From
    13 -> SumD PPViewHashesDontMatch <! From <! From
    14 -> SumD UnspendableUTxONoDatumHash <! From
    15 -> SumD ExtraRedeemers <! From
    16 -> SumD MalformedScriptWitnesses <! From
    17 -> SumD MalformedReferenceScripts <! From
    18 -> SumD SubsInSubsUtxowFailure
    n -> Invalid n

-- =====================================================
-- Injecting from one PredicateFailure to another

babbageToBabelUtxowPredFailure ::
  forall era.
  BabbageUtxowPredFailure era ->
  BabelUtxowPredFailure era
babbageToBabelUtxowPredFailure = \case
  Babbage.AlonzoInBabbageUtxowPredFailure x -> alonzoToBabelUtxowPredFailure x
  Babbage.UtxoFailure x -> UtxoFailure x
  Babbage.MalformedScriptWitnesses xs -> MalformedScriptWitnesses xs
  Babbage.MalformedReferenceScripts xs -> MalformedReferenceScripts xs

alonzoToBabelUtxowPredFailure ::
  forall era.
  AlonzoUtxowPredFailure era ->
  BabelUtxowPredFailure era
alonzoToBabelUtxowPredFailure = \case
  Alonzo.ShelleyInAlonzoUtxowPredFailure f -> shelleyToBabelUtxowPredFailure f
  Alonzo.MissingRedeemers rs -> MissingRedeemers rs
  Alonzo.MissingRequiredDatums mds rds -> MissingRequiredDatums mds rds
  Alonzo.NotAllowedSupplementalDatums uds ads -> NotAllowedSupplementalDatums uds ads
  Alonzo.PPViewHashesDontMatch a b -> PPViewHashesDontMatch a b
  Alonzo.MissingRequiredSigners _xs ->
    error "Impossible case. It will be removed once we are in Babel. See #3972"
  Alonzo.UnspendableUTxONoDatumHash ins -> UnspendableUTxONoDatumHash ins
  Alonzo.ExtraRedeemers xs -> ExtraRedeemers xs

shelleyToBabelUtxowPredFailure :: ShelleyUtxowPredFailure era -> BabelUtxowPredFailure era
shelleyToBabelUtxowPredFailure = \case
  Shelley.InvalidWitnessesUTXOW xs -> InvalidWitnessesUTXOW xs
  Shelley.MissingVKeyWitnessesUTXOW xs -> MissingVKeyWitnessesUTXOW xs
  Shelley.MissingScriptWitnessesUTXOW xs -> MissingScriptWitnessesUTXOW xs
  Shelley.ScriptWitnessNotValidatingUTXOW xs -> ScriptWitnessNotValidatingUTXOW xs
  Shelley.UtxoFailure x -> UtxoFailure x
  Shelley.MIRInsufficientGenesisSigsUTXOW _xs ->
    error "Impossible: MIR has been removed in Babel"
  Shelley.MissingTxBodyMetadataHash x -> MissingTxBodyMetadataHash x
  Shelley.MissingTxMetadata x -> MissingTxMetadata x
  Shelley.ConflictingMetadataHash a b -> ConflictingMetadataHash a b
  Shelley.InvalidMetadata -> InvalidMetadata
  Shelley.ExtraneousScriptWitnessesUTXOW xs -> ExtraneousScriptWitnessesUTXOW xs

------------

{- CIP-0118#UTXOW-rule

Jump to CIP-0118#UTXO-rule to continue... -}

-- | UTXOW transition rule that is used in Babel era.
babelUtxowTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , Environment (EraRule "UTXOW" era) ~ BabelUtxoEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
  , Environment (EraRule "UTXO" era) ~ BabelUtxoEnv era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , InjectRuleFailure "UTXOW" BabelUtxowPredFailure era
  , BabelEraTxBody era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , BabbageEraTxBody era
  ) =>
  TransitionRule (EraRule "UTXOW" era)
babelUtxowTransition = do
  TRC (utxoEnv@(BabelUtxoEnv _ pp certState bobs batchData), u, tx) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses tx
      inputs = (txBody ^. referenceInputsTxBodyL) `Set.union` (txBody ^. inputsTxBodyL)

  -- check scripts
  {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let scriptsNeeded = getScriptsNeeded utxo txBody -- TODO WG
      scriptsProvided = getScriptsProvided utxo tx
      scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded
  {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
  -- CHANGED In BABBAGE txscripts depends on UTxO
  runTest $ validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

  {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
  let sReceived = Map.keysSet $ tx ^. witsTxL . scriptTxWitsL
      sRefs = Map.keysSet $ getReferenceScripts utxo inputs
  runTest $ babbageMissingScripts pp scriptHashesNeeded sRefs sReceived

  {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
  runTest $ missingRequiredDatums utxo tx

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

  -- check VKey witnesses
  -- let txbodyHash = hashAnnotated @(Crypto era) txbody
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsKeyHashes certState utxo txBody

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $ Shelley.validateMetadata pp tx

  {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
                         x ∈ Script ∪ Datum ⇒ isWellFormed x
  -}
  runTest $ validateScriptsWellFormed pp tx
  -- Note that Datum validation is done during deserialization,
  -- as given by the decoders in the Plutus libraray

  {- languages tx utxo ⊆ dom(costmdls pp) -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'.

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  -- TODO allowed languages?
  runTest $ ppViewHashesMatch tx pp scriptsProvided scriptHashesNeeded

  runTest $ chkRequiredBatchObservers (tx ^. bodyTxL . requireBatchObserversTxBodyL) bobs
  runTest $ noSubsInSubs batchData tx

  trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)

chkRequiredBatchObservers ::
  Set (ScriptHash (EraCrypto era)) ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (BabelUtxowPredFailure era)
chkRequiredBatchObservers requiredBatchObservers bobs = failureUnless check BatchObserversFailure
  where
    check = requiredBatchObservers `Set.isSubsetOf` bobs

noSubsInSubs ::
  (EraTx era, BabelEraTxBody era) => BatchData era -> Tx era -> Test (BabelUtxowPredFailure era)
noSubsInSubs bd tx = failureUnless check SubsInSubsUtxowFailure
  where
    check = case (isTop bd tx, toList (tx ^. bodyTxL . swapsTxBodyL)) of
      (True, _) -> True
      (False, []) -> True
      (False, _) -> False

{- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} ⊆ dom(txdats txw)   -}
{- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h, ) ∈ txouts tx ∪ utxo (refInputs tx) } -}
missingRequiredDatums ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , BabelEraTxBody era
  ) =>
  UTxO era ->
  Tx era ->
  Test (BabelUtxowPredFailure era)
missingRequiredDatums utxo tx = do
  let txBody = tx ^. bodyTxL
      scriptsProvided = getScriptsProvided utxo tx
      (inputHashes, txInsNoDataHash) = getInputDataHashesTxBody utxo txBody scriptsProvided
      spendOutHashes = Set.unions $ fmap (getDataHashFromTxOut . sizedValue) (toList (txBody ^. spendOutsTxBodyL))
      spentHashes = inputHashes <> spendOutHashes
      txHashes = domain (unTxDats $ tx ^. witsTxL . datsTxWitsL)
      unmatchedDatumHashes = eval (spentHashes ➖ txHashes)
      allowedSupplementalDataHashes = getSupplementalDataHashes utxo txBody
      supplimentalDatumHashes = eval (txHashes ➖ spentHashes)
      (okSupplimentalDHs, notOkSupplimentalDHs) =
        Set.partition (`Set.member` allowedSupplementalDataHashes) supplimentalDatumHashes
  sequenceA_
    [ failureUnless
        (Set.null txInsNoDataHash)
        (UnspendableUTxONoDatumHash @era txInsNoDataHash)
    , failureUnless
        (Set.null unmatchedDatumHashes)
        (MissingRequiredDatums unmatchedDatumHashes txHashes)
    , failureUnless
        (Set.null notOkSupplimentalDHs)
        (NotAllowedSupplementalDatums notOkSupplimentalDHs okSupplimentalDHs)
    ]

getDataHashFromTxOut :: AlonzoEraTxOut era => TxOut era -> Set (DataHash (EraCrypto era))
getDataHashFromTxOut txOut = case txOut ^. datumTxOutF of
  DatumHash dh -> Set.singleton dh
  _ -> Set.empty

getInputDataHashesTxBody ::
  forall era.
  (EraTxBody era, AlonzoEraTxOut era, EraScript era) =>
  UTxO era ->
  TxBody era ->
  ScriptsProvided era ->
  (Set.Set (DataHash (EraCrypto era)), Set.Set (TxIn (EraCrypto era)))
getInputDataHashesTxBody (UTxO mp) txBody (ScriptsProvided scriptsProvided) =
  Map.foldlWithKey' accum (Set.empty, Set.empty) spendUTxO
  where
    spendInputs :: Set (TxIn (EraCrypto era))
    spendInputs = txBody ^. inputsTxBodyL
    spendUTxO :: Map.Map (TxIn (EraCrypto era)) (TxOut era)
    spendUTxO = eval (spendInputs ◁ mp)
    accum ans@(!hashSet, !inputSet) txIn txOut =
      let addr = txOut ^. addrTxOutL
          isTwoPhaseScriptAddress = isTwoPhaseScriptAddressFromMap scriptsProvided addr
       in case txOut ^. datumTxOutF of
            NoDatum
              | isTwoPhaseScriptAddress -> (hashSet, Set.insert txIn inputSet)
            DatumHash dataHash
              | isTwoPhaseScriptAddress -> (Set.insert dataHash hashSet, inputSet)
            -- Though it is somewhat odd to allow non-two-phase-scripts to include a datum,
            -- the Alonzo era already set the precedent with datum hashes, and several dapp
            -- developers see this as a helpful feature.
            _ -> ans

-- spendInputs2 :: Set (TxIn (EraCrypto era))
-- spendInputs2 =
--   eval $
--     (Map.elems $ Map.restrictKeys mp (txBody ^. inputsTxBodyL))
--       <> (fmap sizedValue . toList $ txBody ^. spendOutsTxBodyL)

-- To Babel Fees implementers: This function is NOT in the right place.
-- Given more time, I'd do something with the EraUTxO class.
-- validateNeededWitnessesFrxo ::
--   forall era.
--   EraUTxO era =>
--   -- | Provided witness
--   Set (KeyHash 'Witness (EraCrypto era)) ->
--   CertState era ->
--   UTxO era ->
--   TxBody era ->
--   Validation (NonEmpty (ShelleyUtxowPredFailure era)) ()
-- validateNeededWitnessesFrxo witsKeyHashes certState utxo txBody =
--   let needed = getWitsVKeyNeeded certState utxo txBody
--       missingWitnesses = Set.difference needed witsKeyHashes
--    in failureUnless (Set.null missingWitnesses) $
--         Shelley.MissingVKeyWitnessesUTXOW @era missingWitnesses

-- babelUtxowTransition ::
--   forall era.
--   ( AlonzoEraTx era
--   , AlonzoEraUTxO era
--   , ScriptsNeeded era ~ AlonzoScriptsNeeded era
--   , ConwayEraTxBody era
--   , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
--   , Environment (EraRule "UTXOW" era) ~ BabelUtxoEnv era
--   , Signal (EraRule "UTXOW" era) ~ Tx era
--   , State (EraRule "UTXOW" era) ~ UTxOState era
--   , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
--   , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
--   , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
--   , -- Allow UTXOW to call UTXO
--     Embed (EraRule "UTXO" era) (EraRule "UTXOW" era)
--   , Environment (EraRule "UTXO" era) ~ BabelUtxoEnv era
--   , Signal (EraRule "UTXO" era) ~ Tx era
--   , State (EraRule "UTXO" era) ~ UTxOState era
--   ) =>
--   TransitionRule (EraRule "UTXOW" era)
-- babelUtxowTransition = do
--   TRC (utxoEnv@(BabelUtxoEnv _ pp certState batchData), u, tx) <- judgmentContext
--   -- TODO allowed languages?

--   {-  (utxo,_,_,_ ) := utxoSt  -}
--   {-  txb := txbody tx  -}
--   {-  txw := txwits tx  -}
--   {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
--   let utxo = utxosUtxo u
--       txBody = tx ^. bodyTxL
--       witsKeyHashes = witsFromTxWitnesses tx
--       inputs =
--         (txBody ^. referenceInputsTxBodyL)
--           `Set.union` (txBody ^. inputsTxBodyL)

--   -- check scripts
--   {- neededHashes := {h | ( , h) ∈ scriptsNeeded utxo txb} -}
--   {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
--   let scriptsNeeded = getScriptsNeeded utxo txBody
--       scriptsProvided = getScriptsProvided utxo tx
--       scriptHashesNeeded = getScriptsHashesNeeded scriptsNeeded
--   {- ∀s ∈ (txscripts txw utxo neededHashes ) ∩ Scriptph1 , validateScript s tx -}
--   -- CHANGED In BABBAGE txscripts depends on UTxO
--   runTest $ validateFailedBabbageScripts tx scriptsProvided scriptHashesNeeded

--   {- neededHashes − dom(refScripts tx utxo) = dom(txwitscripts txw) -}
--   let sReceived = Map.keysSet $ tx ^. witsTxL . scriptTxWitsL
--       sRefs =
--         Map.keysSet (getReferenceScripts utxo inputs)

--   runTest $ babbageMissingScripts pp scriptHashesNeeded sRefs sReceived

--   {-  inputHashes ⊆  dom(txdats txw) ⊆  allowed -}
--   runTest $ missingRequiredDatums utxo tx

--   {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
--                            h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
--   runTest $ hasExactSetOfRedeemers tx scriptsProvided scriptsNeeded

--   -- TODO WG: This probably isn't exactly right, but it's close enough for now
--   -- ∀[ (vk , σ) ∈ vkSigs ] isSigned vk (txidBytes (tx .Tx.body .TxBody.txid) + sumReqs (tx .requiredTxs)) σ
--   -- check VKey witnesses
--   -- let txbodyHash = hashAnnotated @(Crypto era) txbody
--   {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
--   runTestOnSignal $ validateVerifiedWits tx

--   {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
--   runTestOnSignal $ validateNeededWitnessesFrxo witsKeyHashes certState utxo txBody -- TODO WG what should this actually do differently?

--   -- check metadata hash
--   {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
--   {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
--   runTestOnSignal $ Shelley.validateMetadata pp tx

--   {- ∀x ∈ range(txdats txw) ∪ range(txwitscripts txw) ∪ (⋃ ( , ,d,s) ∈ txouts tx {s, d}),
--                          x ∈ Script ∪ Datum ⇒ isWellFormed x
--   -}
--   runTest $ validateScriptsWellFormed pp tx
--   -- Note that Datum validation is done during deserialization,
--   -- as given by the decoders in the Plutus libraray

--   {- languages tx utxo ⊆ dom(costmdls pp) -}
--   -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
--   -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'.

--   {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
--   runTest $ ppViewHashesMatch tx pp scriptsProvided scriptHashesNeeded

--   trans @(EraRule "UTXO" era) $ TRC (utxoEnv, u, tx)