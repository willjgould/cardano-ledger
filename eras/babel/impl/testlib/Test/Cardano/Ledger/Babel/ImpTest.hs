{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Babel.ImpTest where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN, Signable, seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash, HashAlgorithm)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress (..),
  RewardAccount (..),
  bootstrapKeyHash,
 )
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript,
  ExUnits (ExUnits),
  plutusScriptLanguage,
  toAsItem,
  toAsIx,
 )
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core hiding (proposals)
import Cardano.Ledger.Babel.TxCert (
  BabelEraTxCert,
  Delegatee (..),
  pattern AuthCommitteeHotKeyTxCert,
  pattern RegDRepTxCert,
  pattern RegDepositDelegTxCert,
  pattern ResignCommitteeColdTxCert,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Globals,
  Network (..),
  ProtVer (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  Version,
  addEpochInterval,
  inject,
  succVersion,
  textToUrl,
 )
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  certDStateL,
  csCommitteeCredsL,
  dsUnifiedL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (ConwayEraPParams, ConwayEraTxBody (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), getLanguageView)
import Cardano.Ledger.Conway.Rules (
  ConwayGovEvent,
  EnactSignal,
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  prevActionAsExpected,
  spoAccepted,
  spoAcceptedRatio,
  validCommitteeTerm,
  withdrawalCanWithdraw,
 )
import Cardano.Ledger.Conway.TxCert (ConwayEraTxCert (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..), credToText)
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.DRep
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash,
  KeyRole (..),
  VerKeyVRF,
  bootstrapWitKeyHash,
  hashKey,
  makeBootstrapWitness,
  witVKeyHash,
 )
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (
  Exception (..),
  SomeException (..),
  catchAny,
  catchAnyDeep,
  evaluateDeep,
  throwIO,
 )

import Cardano.Crypto.VRF.Class (VRFAlgorithm)
import Cardano.Ledger.Babbage.Tx (hashScriptIntegrity)
import Cardano.Ledger.Keys (WitVKey (..))
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.Mary (Mary, MaryValue (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Datum (..))
import qualified Cardano.Ledger.Plutus.Data as PlutusData
import Cardano.Ledger.Plutus.Language (Language (PlutusV1), Plutus, PlutusLanguage, SLanguage (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash, extractHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (PoolParams (..), ShelleyTxOut (ShelleyTxOut), TxIn (..))
import Cardano.Ledger.Shelley.AdaPots (sumAdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.HardForks as HardForks (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  IncrementalStake (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  asTreasuryL,
  certVStateL,
  consumed,
  curPParamsEpochStateL,
  epochStateGovStateL,
  epochStateIncrStakeDistrL,
  epochStateUMapL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  nesPdL,
  newEpochStateGovStateL,
  prevPParamsEpochStateL,
  produced,
  smartUTxOState,
  startStep,
  utxosDonationL,
  utxosGovStateL,
  utxosStakeDistrL,
  utxosUtxoL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.UTxO (produced)
import Cardano.Ledger.Tools (calcMinFeeTxNativeScriptWits, integralToByteStringN)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (RDPair (..))
import Cardano.Ledger.UMap as UMap
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO (..),
  txinLookup,
 )
import Cardano.Ledger.Val (Val (..))
import Control.Exception (Exception (..), SomeException)
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadState (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), gets, modify)
import Control.Monad.Writer (MonadWriter (..))
import Control.Monad.Writer.Class (MonadWriter (..))
import Control.State.Transition (STS (..), TRC (..), applySTSOptsEither)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  AssertionPolicy (..),
  STS (..),
  SingEP (..),
  ValidationPolicy (..),
 )
import Data.Coerce (coerce)
import Data.Data (Data (..), Proxy (..), Typeable, type (:~:) (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..), toList)
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree
import Data.Type.Equality (TestEquality (..))
import Debug.Trace (trace)
import qualified GHC.Exts as GHC (fromList)
import GHC.Stack (SrcLoc (..), callStack, getCallStack)
import GHC.TypeLits (KnownSymbol, Natural, Symbol, symbolVal)
import Lens.Micro
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=))
import Numeric.Natural (Natural)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.String (renderString)
import System.Random
import qualified System.Random as Random
import Test.Cardano.Ledger.Babbage.ImpTest (
  AlonzoEraImp,
  MaryEraImp,
  SomeSTSEvent (..),
  alonzoFixupTx,
  impAllegraSatisfyNativeScript,
  initAlonzoImpNES,
  scriptTestContexts,
 )
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Babel.TreeDiff ()
import Test.Cardano.Ledger.Conway.ImpTest (
  ConwayEraImp,
  initShelleyImpNES,
  plutusTestScripts,
 )
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraExpectation)
import Test.Cardano.Ledger.Core.KeyPair (
  ByronKeyPair (..),
  KeyPair (..),
  mkAddr,
  mkKeyHash,
  mkWitnessesVKey,
 )
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals, txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs (..), ScriptTestContext (..), testingCostModels)
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestEnv (..),
  ImpTestM (..),
  ImpTestState (..),
  ShelleyEraImp (..),
 )
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec.Core.Spec (Example (..), Params, paramsQuickCheckArgs)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (QCGen (..), integerVariant, mkQCGen)
import Type.Reflection (Typeable, typeOf)

instance ShelleyEraImp (BabelEra StandardCrypto) where
  initImpTestState = do
    kh <- fst <$> freshKeyPair
    let committee = Committee [(KeyHashObj kh, EpochNo 15)] (1 %! 1)
    anchor <- arbitrary
    let constitution = Constitution anchor SNothing
    impNESL %= initConwayNES committee constitution
    where
      initConwayNES committee constitution nes =
        let newNes =
              (initAlonzoImpNES nes)
                & nesEsL
                . curPParamsEpochStateL
                . ppDRepActivityL
                .~ EpochInterval 100
                & nesEsL
                . curPParamsEpochStateL
                . ppGovActionLifetimeL
                .~ EpochInterval 30
                & nesEsL
                . curPParamsEpochStateL
                . ppGovActionDepositL
                .~ Coin 123
                & nesEsL
                . curPParamsEpochStateL
                . ppCommitteeMaxTermLengthL
                .~ EpochInterval 20
                & nesEsL
                . curPParamsEpochStateL
                . ppCommitteeMinSizeL
                .~ 1
                & nesEsL
                . curPParamsEpochStateL
                . ppDRepVotingThresholdsL
                %~ ( \dvt ->
                      dvt
                        { dvtCommitteeNormal = 1 %! 1
                        , dvtCommitteeNoConfidence = 1 %! 2
                        , dvtUpdateToConstitution = 1 %! 2
                        }
                   )
                & nesEsL
                . epochStateGovStateL
                . committeeGovStateL
                .~ SJust committee
                & nesEsL
                . epochStateGovStateL
                . constitutionGovStateL
                .~ constitution
            epochState = newNes ^. nesEsL
            ratifyState =
              def
                & rsEnactStateL
                .~ mkEnactState (epochState ^. epochStateGovStateL)
         in newNes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = fxupTx

instance MaryEraImp (BabelEra StandardCrypto)

instance ShelleyEraImp (BabelEra StandardCrypto) => AlonzoEraImp (BabelEra StandardCrypto) where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3
      <> plutusTestScripts SPlutusV4

instance ConwayEraImp (BabelEra StandardCrypto)

-- | Modify the PParams in the current state with the given function
conwayModifyPParams ::
  (PParams (BabelEra StandardCrypto) -> PParams (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
conwayModifyPParams f = modifyNES $ \nes ->
  nes
    & nesEsL
    . curPParamsEpochStateL
    %~ f
    & newEpochStateGovStateL
    . drepPulsingStateGovStateL
    %~ modifyDRepPulser
  where
    modifyDRepPulser pulser =
      case finishDRepPulser pulser of
        (snapshot, ratifyState) ->
          DRComplete snapshot (ratifyState & rsEnactStateL . ensCurPParamsL %~ f)

withImpStateWithProtVer ::
  Version ->
  SpecWith (ImpTestState (BabelEra StandardCrypto)) ->
  Spec
withImpStateWithProtVer ver = do
  withImpStateModified $
    impNESL
      . nesEsL
      . esLStateL
      . lsUTxOStateL
      . (utxosGovStateL @(BabelEra StandardCrypto))
      . cgsCurPParamsL
      %~ ( \(PParams pp) ->
            PParams (pp {cppProtocolVersion = ProtVer ver 0})
         )

fxupTx ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fxupTx =
  addNativeScriptTxWits
    >=> addCollateralInput
    >=> addRootTxIn
    -- We need to update the indices after adding the rootTxIn because the
    -- indices of inputs might get bumped if the rootTxIn appears before them
    >=> fixupRedeemerIndices
    >=> fixupRedeemers
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
    >=> fixupPPHash
    >=> fixupFees
    >=> updateAddrTxWits

fixupScriptWits ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupScriptWits tx = impAnn "fixupScriptWits" $ do
  contexts <- impGetPlutusContexts tx
  utxo <- getUTxO
  let ScriptsProvided provided = getScriptsProvided utxo tx
  let contextsToAdd = filter (\(_, sh, _) -> not (Map.member sh provided)) contexts
  let
    plutusToScript ::
      forall l.
      PlutusLanguage l =>
      Plutus l ->
      ImpTestM (BabelEra StandardCrypto) (Script (BabelEra StandardCrypto))
    plutusToScript p =
      case mkPlutusScript @(BabelEra StandardCrypto) p of
        Just x -> pure $ fromPlutusScript x
        Nothing -> error "Plutus version not supported by (BabelEra StandardCrypto)"
  scriptWits <- forM contextsToAdd $ \(_, sh, ScriptTestContext plutus _) ->
    (sh,) <$> plutusToScript plutus
  pure $
    tx
      & witsTxL
      . scriptTxWitsL
      <>~ Map.fromList scriptWits

fixupDatums ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupDatums tx = impAnn "fixupDatums" $ do
  contexts <- impGetPlutusContexts tx
  let purposes = (^. _1) <$> contexts
  datums <- traverse collectDatums purposes
  let TxDats prevDats = tx ^. witsTxL . datsTxWitsL
  pure $
    tx
      & witsTxL
      . datsTxWitsL
      .~ TxDats
        (Map.union prevDats $ fromElems PlutusData.hashData (catMaybes datums))
  where
    collectDatums ::
      PlutusPurpose AsIxItem (BabelEra StandardCrypto) ->
      ImpTestM (BabelEra StandardCrypto) (Maybe (PlutusData.Data (BabelEra StandardCrypto)))
    collectDatums purpose = do
      let txIn = unAsItem <$> toSpendingPurpose (hoistPlutusPurpose toAsItem purpose)
      txOut <- traverse impLookupUTxO txIn
      pure $ getData =<< txOut

    getData :: TxOut (BabelEra StandardCrypto) -> Maybe (PlutusData.Data (BabelEra StandardCrypto))
    getData txOut = case txOut ^. datumTxOutF of
      DatumHash _dh ->
        spendDatum
          <$> Map.lookup (txOutScriptHash txOut) scriptTestContexts2
      _ -> Nothing

    txOutScriptHash txOut
      | Addr _ (ScriptHashObj sh) _ <- txOut ^. addrTxOutL = sh
      | otherwise = error "TxOut does not have a payment script"

    spendDatum (ScriptTestContext _ (PlutusArgs _ (Just d))) = PlutusData.Data d
    spendDatum _ = error "Context does not have a spending datum"

scriptTestContexts2 ::
  Map (ScriptHash (EraCrypto (BabelEra StandardCrypto))) ScriptTestContext
scriptTestContexts2 =
  plutusTestScripts SPlutusV1
    <> plutusTestScripts SPlutusV2
    <> plutusTestScripts SPlutusV3
    <> plutusTestScripts SPlutusV4

impLookupPlutusScriptMaybe ::
  ScriptHash (EraCrypto (BabelEra StandardCrypto)) ->
  Maybe (PlutusScript (BabelEra StandardCrypto))
impLookupPlutusScriptMaybe sh =
  (\(ScriptTestContext plutus _) -> mkPlutusScript plutus) =<< impGetScriptContextMaybe sh

fixupPPHash ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupPPHash tx = impAnn "fixupPPHash" $ do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  let
    scriptHashes :: Set (ScriptHash (EraCrypto (BabelEra StandardCrypto)))
    scriptHashes = getScriptsHashesNeeded . getScriptsNeeded utxo $ tx ^. bodyTxL
    plutusLanguage sh = do
      let mbyPlutus = impLookupPlutusScriptMaybe sh
      pure $ getLanguageView pp . plutusScriptLanguage @(BabelEra StandardCrypto) <$> mbyPlutus
  langs <- traverse plutusLanguage $ Set.toList scriptHashes
  let
    integrityHash =
      hashScriptIntegrity
        (Set.fromList $ catMaybes langs)
        (tx ^. witsTxL . rdmrsTxWitsL)
        (tx ^. witsTxL . datsTxWitsL)
  pure $
    tx
      & bodyTxL
      . scriptIntegrityHashTxBodyL
      .~ integrityHash

fixupOutputDatums ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupOutputDatums tx = impAnn "fixupOutputDatums" $ do
  let
    isDatum (Datum _) = True
    isDatum _ = False
    addDatum txOut =
      case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj sh) _ -> do
          case impGetScriptContextMaybe sh of
            Just (ScriptTestContext _ (PlutusArgs _ mbySpendDatum))
              | not $ isDatum (txOut ^. datumTxOutF) -> do
                  spendDatum <-
                    impAnn "Looking up spend datum" $
                      expectJust mbySpendDatum
                  pure $
                    txOut
                      & dataHashTxOutL
                      .~ SJust (PlutusData.hashData @(BabelEra StandardCrypto) $ PlutusData.Data spendDatum)
            _ -> pure txOut
        _ -> pure txOut
  newOutputs <- traverse addDatum $ tx ^. bodyTxL . outputsTxBodyL
  pure $
    tx
      & bodyTxL
      . outputsTxBodyL
      .~ newOutputs

fixupRedeemers ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupRedeemers tx = impAnn "fixupRedeemers" $ do
  contexts <- impGetPlutusContexts tx
  exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
  let
    mkNewRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose toAsIx prpIdx, (PlutusData.Data dat, exUnits))
    Redeemers oldRedeemers = tx ^. witsTxL . rdmrsTxWitsL
    newRedeemers = Map.fromList (mkNewRedeemers <$> contexts)
  pure $
    tx
      & witsTxL
      . rdmrsTxWitsL
      .~ Redeemers (Map.union oldRedeemers newRedeemers)

impGetPlutusContexts ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    [ ( PlutusPurpose AsIxItem (BabelEra StandardCrypto)
      , ScriptHash (EraCrypto (BabelEra StandardCrypto))
      , ScriptTestContext
      )
    ]
impGetPlutusContexts tx = do
  let txBody = tx ^. bodyTxL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let AlonzoScriptsNeeded asn = getScriptsNeeded utxo txBody
  mbyContexts <- forM asn $ \(prp, sh) -> do
    pure $ (prp,sh,) <$> impGetScriptContextMaybe sh
  pure $ catMaybes mbyContexts

impGetScriptContextMaybe ::
  ScriptHash (EraCrypto (BabelEra StandardCrypto)) ->
  Maybe ScriptTestContext
impGetScriptContextMaybe sh = Map.lookup sh scriptTestContexts2

fixupRedeemerIndices ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupRedeemerIndices tx = impAnn "fixupRedeemerIndices" $ do
  (rootTxIn, _) <- lookupImpRootTxOut
  let
    txInputs = tx ^. bodyTxL . inputsTxBodyL
    rootTxIndex = toEnum $ Set.findIndex rootTxIn txInputs
    updateIndex (SpendingPurpose (AsIx i))
      | i >= rootTxIndex = SpendingPurpose . AsIx $ succ i
    updateIndex x = x
  pure $
    tx
      & witsTxL
      . rdmrsTxWitsL
      %~ (\(Redeemers m) -> Redeemers $ Map.mapKeys updateIndex m)

addCollateralInput ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
addCollateralInput tx = impAnn "addCollateralInput" $ do
  ctx <- impGetPlutusContexts tx
  if null ctx
    then pure tx
    else do
      collateralInput <- makeCollateralInput
      pure $
        tx
          & bodyTxL
          . collateralInputsTxBodyL
          <>~ Set.singleton collateralInput

makeCollateralInput ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
makeCollateralInput = do
  -- TODO: make more accurate
  let collateral = Coin 10_000_000
  (_, addr) <- freshKeyAddr
  withFixup fxupTx $ sendCoinTo addr collateral

makeCollateralInputAccurate ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Integer ->
  ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
makeCollateralInputAccurate c = do
  (_, addr) <- freshKeyAddr
  withFixup fxupTx $ sendCoinTo addr (Coin c)

registerInitialCommittee ::
  HasCallStack =>
  ImpTestM
    (BabelEra StandardCrypto)
    (NonEmpty (Credential 'HotCommitteeRole (EraCrypto (BabelEra StandardCrypto))))
registerInitialCommittee = do
  committeeMembers <- Set.toList <$> getCommitteeMembers
  case committeeMembers of
    x : xs -> traverse registerCommitteeHotKey $ x NE.:| xs
    _ -> error "Expected an initial committee"

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep ::
  ImpTestM (BabelEra StandardCrypto) (KeyHash 'DRepRole (EraCrypto (BabelEra StandardCrypto)))
registerDRep = do
  -- Register a DRep
  khDRep <- freshKeyHash
  submitTxAnn_ "Register DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton
        ( RegDRepTxCert
            (KeyHashObj khDRep)
            zero
            SNothing
        )
  dreps <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
  pure khDRep

-- | In contrast to `setupSingleDRep`, this function does not make a UTxO entry
-- that could count as delegated stake to the DRep, so that we can test that
-- rewards are also calculated nonetheless.
setupDRepWithoutStake ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( KeyHash 'DRepRole (EraCrypto (BabelEra StandardCrypto))
    , KeyHash 'Staking (EraCrypto (BabelEra StandardCrypto))
    )
setupDRepWithoutStake = do
  drepKH <- registerDRep
  delegatorKH <- freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.fromList
        [ mkRegDepositDelegTxCert @(BabelEra StandardCrypto)
            (KeyHashObj delegatorKH)
            (DelegVote (DRepCredential $ KeyHashObj drepKH))
            deposit
        ]
  pure (drepKH, delegatorKH)

-- | Registers a new DRep and delegates the specified amount of ADA to it.
setupSingleDRep ::
  Integer ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto))
    , Credential 'Staking (EraCrypto (BabelEra StandardCrypto))
    , KeyPair 'Payment (EraCrypto (BabelEra StandardCrypto))
    )
setupSingleDRep stake = do
  drepKH <- registerDRep
  (delegatorKH, delegatorKP) <- freshKeyPair
  (_, spendingKP) <- freshKeyPair
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . outputsTxBodyL
      .~ SSeq.singleton
        ( mkBasicTxOut
            (mkAddr (spendingKP, delegatorKP))
            (inject $ Coin stake)
        )
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.fromList
        [ mkRegDepositDelegTxCert @(BabelEra StandardCrypto)
            (KeyHashObj delegatorKH)
            (DelegVote (DRepCredential $ KeyHashObj drepKH))
            zero
        ]
  pure (KeyHashObj drepKH, KeyHashObj delegatorKH, spendingKP)

getsPParams ::
  Lens' (PParams (BabelEra StandardCrypto)) a ->
  ImpTestM (BabelEra StandardCrypto) a
getsPParams f = getsNES $ nesEsL . curPParamsEpochStateL . f

-- | Sets up a stake pool with coin delegated to it.
--
-- NOTE: This uses the `RegDepositDelegTxCert` for delegating, so it has to be
-- in Babel. The Shelley version of this function would have to separately
-- register the staking credential and then delegate it.
setupPoolWithStake ::
  Coin ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( KeyHash 'StakePool (EraCrypto (BabelEra StandardCrypto))
    , Credential 'Payment (EraCrypto (BabelEra StandardCrypto))
    , Credential 'Staking (EraCrypto (BabelEra StandardCrypto))
    )
setupPoolWithStake delegCoin = do
  khPool <- registerPool
  credDelegatorPayment <- KeyHashObj <$> freshKeyHash
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
  void $
    sendCoinTo
      (Addr Testnet credDelegatorPayment (StakeRefBase credDelegatorStaking))
      delegCoin
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  submitTxAnn_ "Delegate to stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.fromList
        [ RegDepositDelegTxCert
            credDelegatorStaking
            (DelegStake khPool)
            (pp ^. ppKeyDepositL)
        ]
  pure (khPool, credDelegatorPayment, credDelegatorStaking)

setupPoolWithoutStake ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( KeyHash 'StakePool (EraCrypto (BabelEra StandardCrypto))
    , Credential 'Staking (EraCrypto (BabelEra StandardCrypto))
    )
setupPoolWithoutStake = do
  khPool <- registerPool
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  submitTxAnn_ "Delegate to stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.fromList
        [ RegDepositDelegTxCert
            credDelegatorStaking
            (DelegStake khPool)
            deposit
        ]
  pure (khPool, credDelegatorStaking)

-- | Submits a transaction with a Vote for the given governance action as
-- some voter
submitVote ::
  HasCallStack =>
  Vote ->
  Voter (EraCrypto (BabelEra StandardCrypto)) ->
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (TxId (EraCrypto (BabelEra StandardCrypto)))
submitVote vote voter gaId = trySubmitVote vote voter gaId >>= expectRightDeep

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
submitYesVote_ ::
  HasCallStack =>
  Voter (EraCrypto (BabelEra StandardCrypto)) ->
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitYesVote_ voter gaId = void $ submitVote VoteYes voter gaId

submitVote_ ::
  HasCallStack =>
  Vote ->
  Voter (EraCrypto (BabelEra StandardCrypto)) ->
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitVote_ vote voter gaId = void $ submitVote vote voter gaId

submitFailingVote ::
  HasCallStack =>
  Voter (EraCrypto (BabelEra StandardCrypto)) ->
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingVote voter gaId expectedFailure =
  trySubmitVote VoteYes voter gaId >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter, and expects an `Either` result.
trySubmitVote ::
  Vote ->
  Voter (EraCrypto (BabelEra StandardCrypto)) ->
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (TxId (EraCrypto (BabelEra StandardCrypto)))
    )
trySubmitVote vote voter gaId =
  fmap (fmap txIdTx) $
    trySubmitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL
        . votingProceduresTxBodyL
        .~ VotingProcedures
          ( Map.singleton
              voter
              ( Map.singleton
                  gaId
                  ( VotingProcedure
                      { vProcVote = vote
                      , vProcAnchor = SNothing
                      }
                  )
              )
          )

submitProposal_ ::
  HasCallStack =>
  ProposalProcedure (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitProposal_ = void . submitProposal

submitProposal ::
  HasCallStack =>
  ProposalProcedure (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
submitProposal proposal = trySubmitProposal proposal >>= expectRightExpr

submitProposals ::
  HasCallStack =>
  NE.NonEmpty (ProposalProcedure (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (NE.NonEmpty (GovActionId (EraCrypto (BabelEra StandardCrypto))))
submitProposals proposals = do
  curEpochNo <- getsNES nesELL
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  tx <- trySubmitProposals proposals >>= expectRightExpr
  let txId = txIdTx tx
      proposalsWithGovActionId =
        NE.zipWith (\idx p -> (GovActionId txId (GovActionIx idx), p)) (0 NE.:| [1 ..]) proposals
  forM proposalsWithGovActionId $ \(govActionId, proposal) -> do
    govActionState <- getGovActionState govActionId
    govActionState
      `shouldBeExpr` GovActionState
        { gasId = govActionId
        , gasCommitteeVotes = mempty
        , gasDRepVotes = mempty
        , gasStakePoolVotes = mempty
        , gasProposalProcedure = proposal
        , gasProposedIn = curEpochNo
        , gasExpiresAfter = addEpochInterval curEpochNo (pp ^. ppGovActionLifetimeL)
        }
    pure govActionId

-- | Submits a transaction that proposes the given proposal
trySubmitProposal ::
  ProposalProcedure (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (GovActionId (EraCrypto (BabelEra StandardCrypto)))
    )
trySubmitProposal proposal = do
  res <- trySubmitProposals (pure proposal)
  pure $ case res of
    Right tx ->
      Right
        GovActionId
          { gaidTxId = txIdTx tx
          , gaidGovActionIx = GovActionIx 0
          }
    Left err -> Left err

trySubmitProposals ::
  NE.NonEmpty (ProposalProcedure (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (Tx (BabelEra StandardCrypto))
    )
trySubmitProposals proposals = do
  trySubmitTx $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . proposalProceduresTxBodyL
      .~ GHC.fromList (toList proposals)

submitFailingProposal ::
  ( ConwayEraTxBody (BabelEra StandardCrypto)
  , HasCallStack
  ) =>
  ProposalProcedure (BabelEra StandardCrypto) ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingProposal proposal expectedFailure =
  trySubmitProposal proposal >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that proposes the given governance action. For proposing
-- multiple actions in the same transaciton use `trySubmitGovActions` instead.
trySubmitGovAction ::
  GovAction (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (GovActionId (EraCrypto (BabelEra StandardCrypto)))
    )
trySubmitGovAction ga = do
  let mkGovActionId tx = GovActionId (txIdTx tx) (GovActionIx 0)
  fmap mkGovActionId <$> trySubmitGovActions (pure ga)

submitAndExpireProposalToMakeReward ::
  Int ->
  Credential 'Staking (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitAndExpireProposalToMakeReward expectedReward stakingC = do
  rewardAccount <- getRewardAccountFor stakingC
  EpochInterval lifetime <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionLifetimeL
  gai <-
    submitProposal $
      ProposalProcedure
        { pProcDeposit = Coin $ fromIntegral expectedReward
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = TreasuryWithdrawals mempty def
        , pProcAnchor = def
        }
  passNEpochs $ 2 + fromIntegral lifetime
  expectMissingGovActionId gai

-- | Submits a transaction that proposes the given governance action
trySubmitGovActions ::
  NE.NonEmpty (GovAction (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (Tx (BabelEra StandardCrypto))
    )
trySubmitGovActions gas = do
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  rewardAccount <- registerRewardAccount
  proposals <- forM gas $ \ga -> do
    pure
      ProposalProcedure
        { pProcDeposit = deposit
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = ga
        , pProcAnchor = def
        }
  trySubmitProposals proposals

submitGovAction ::
  HasCallStack =>
  GovAction (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
submitGovAction ga = do
  gaId NE.:| _ <- submitGovActions (pure ga)
  pure gaId

submitGovAction_ ::
  HasCallStack =>
  GovAction (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitGovAction_ = void . submitGovAction

submitGovActions ::
  HasCallStack =>
  NE.NonEmpty (GovAction (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (NE.NonEmpty (GovActionId (EraCrypto (BabelEra StandardCrypto))))
submitGovActions gas = do
  tx <- trySubmitGovActions gas >>= expectRightExpr
  let txId = txIdTx tx
  pure $ NE.zipWith (\idx _ -> GovActionId txId (GovActionIx idx)) (0 NE.:| [1 ..]) gas

submitTreasuryWithdrawals ::
  [(RewardAccount (EraCrypto (BabelEra StandardCrypto)), Coin)] ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
submitTreasuryWithdrawals wdrls = do
  policy <- getGovPolicy
  submitGovAction $ TreasuryWithdrawals (Map.fromList wdrls) policy

enactTreasuryWithdrawals ::
  [(RewardAccount (EraCrypto (BabelEra StandardCrypto)), Coin)] ->
  Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto)) ->
  Credential 'HotCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
enactTreasuryWithdrawals withdrawals dRep cm = do
  gaId <- submitTreasuryWithdrawals withdrawals
  submitYesVote_ (DRepVoter dRep) gaId
  submitYesVote_ (CommitteeVoter cm) gaId
  passNEpochs 2
  pure gaId

submitParameterChange ::
  StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
  PParamsUpdate (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
submitParameterChange parent ppu = do
  policy <- getGovPolicy
  submitGovAction $ ParameterChange (GovPurposeId <$> parent) ppu policy

getGovPolicy ::
  ImpTestM (BabelEra StandardCrypto) (StrictMaybe (ScriptHash (EraCrypto (BabelEra StandardCrypto))))
getGovPolicy =
  getsNES $
    nesEpochStateL . epochStateGovStateL . constitutionGovStateL . constitutionScriptL

submitFailingGovAction ::
  HasCallStack =>
  GovAction (BabelEra StandardCrypto) ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingGovAction ga expectedFailure = trySubmitGovAction ga >>= (`shouldBeLeftExpr` expectedFailure)

getEnactState ::
  ImpTestM (BabelEra StandardCrypto) (EnactState (BabelEra StandardCrypto))
getEnactState = mkEnactState <$> getsNES (nesEsL . epochStateGovStateL)

getProposals ::
  ImpTestM (BabelEra StandardCrypto) (Proposals (BabelEra StandardCrypto))
getProposals = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL

logProposalsForest :: ImpTestM (BabelEra StandardCrypto) ()
logProposalsForest = do
  proposals <- getProposals
  logEntry $ proposalsShowDebug proposals True

getCommitteeMembers ::
  ImpTestM
    (BabelEra StandardCrypto)
    (Set.Set (Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto))))
getCommitteeMembers = do
  committee <-
    getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
  pure $ Map.keysSet $ foldMap' committeeMembers committee

getLastEnactedCommittee ::
  ImpTestM
    (BabelEra StandardCrypto)
    (StrictMaybe (GovPurposeId 'CommitteePurpose (BabelEra StandardCrypto)))
getLastEnactedCommittee = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grCommitteeL . prRootL

getConstitution ::
  ImpTestM (BabelEra StandardCrypto) (Constitution (BabelEra StandardCrypto))
getConstitution =
  getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL

getLastEnactedConstitution ::
  ImpTestM
    (BabelEra StandardCrypto)
    (StrictMaybe (GovPurposeId 'ConstitutionPurpose (BabelEra StandardCrypto)))
getLastEnactedConstitution = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grConstitutionL . prRootL

getLastEnactedParameterChange ::
  ImpTestM
    (BabelEra StandardCrypto)
    (StrictMaybe (GovPurposeId 'PParamUpdatePurpose (BabelEra StandardCrypto)))
getLastEnactedParameterChange = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grPParamUpdateL . prRootL

getLastEnactedHardForkInitiation ::
  ImpTestM
    (BabelEra StandardCrypto)
    (StrictMaybe (GovPurposeId 'HardForkPurpose (BabelEra StandardCrypto)))
getLastEnactedHardForkInitiation = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grHardForkL . prRootL

getConstitutionProposals ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( Map.Map
        (GovPurposeId 'ConstitutionPurpose (BabelEra StandardCrypto))
        (PEdges (GovPurposeId 'ConstitutionPurpose (BabelEra StandardCrypto)))
    )
getConstitutionProposals = do
  ps <- getProposals
  pure $ ps ^. pGraphL . grConstitutionL . pGraphNodesL

getParameterChangeProposals ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( Map.Map
        (GovPurposeId 'PParamUpdatePurpose (BabelEra StandardCrypto))
        (PEdges (GovPurposeId 'PParamUpdatePurpose (BabelEra StandardCrypto)))
    )
getParameterChangeProposals = do
  ps <- getProposals
  pure $ ps ^. pGraphL . grPParamUpdateL . pGraphNodesL

logProposalsForestDiff ::
  ToExpr (PParamsHKD StrictMaybe (BabelEra StandardCrypto)) =>
  Proposals (BabelEra StandardCrypto) ->
  Proposals (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
logProposalsForestDiff pf1 pf2 = logEntry $ unlines ["Proposals Forest Diff:", diffExpr pf1 pf2]

-- | Looks up the governance action state corresponding to the governance action id
lookupGovActionState ::
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (Maybe (GovActionState (BabelEra StandardCrypto)))
lookupGovActionState aId = proposalsLookupId aId <$> getProposals

-- | Looks up the governance action state corresponding to the governance action id
getGovActionState ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionState (BabelEra StandardCrypto))
getGovActionState govActionId =
  impAnn "Expecting an action state" $ do
    lookupGovActionState govActionId >>= \case
      Nothing ->
        assertFailure $ "Could not find action state for govActionId: " <> show govActionId
      Just govActionState -> pure govActionState

expectPresentGovActionId ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
expectPresentGovActionId govActionId = void $ getGovActionState govActionId

expectMissingGovActionId ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
expectMissingGovActionId govActionId =
  impAnn "Expecting for gov action state to be missing" $ do
    lookupGovActionState govActionId >>= \case
      Just _ ->
        expectationFailure $ "Found gov action state for govActionId: " <> show govActionId
      Nothing -> pure ()

-- | Builds a RatifyEnv from the current state
getRatifyEnv ::
  ImpTestM (BabelEra StandardCrypto) (RatifyEnv (BabelEra StandardCrypto))
getRatifyEnv = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  pure
    RatifyEnv
      { reStakePoolDistr = poolDistr
      , reStakeDistr = credMap stakeDistr
      , reDRepState = drepState
      , reDRepDistr = drepDistr
      , reCurrentEpoch = eNo - 1
      , reCommitteeState = committeeState
      }

ccShouldNotBeExpired ::
  HasCallStack =>
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
ccShouldNotBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (<= ccExpiryEpochNo)

ccShouldBeExpired ::
  HasCallStack =>
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
ccShouldBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (> ccExpiryEpochNo)

getCCExpiry ::
  HasCallStack =>
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) EpochNo
getCCExpiry coldC = do
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  case committee of
    SNothing -> assertFailure "There is no committee"
    SJust Committee {committeeMembers} ->
      case Map.lookup coldC committeeMembers of
        Nothing -> assertFailure $ "Committee not found for cold credential: " <> show coldC
        Just epochNo -> pure epochNo

-- | Test the resignation status for a CC cold key to be resigned
ccShouldBeResigned ::
  HasCallStack =>
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
ccShouldBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  authHk <$> Map.lookup coldK committeeCreds `shouldBe` Just Nothing

-- | Test the resignation status for a CC cold key to not be resigned
ccShouldNotBeResigned ::
  HasCallStack =>
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
ccShouldNotBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  (Map.lookup coldK committeeCreds >>= authHk) `shouldSatisfy` isJust

authHk :: CommitteeAuthorization c -> Maybe (Credential 'HotCommitteeRole c)
authHk (CommitteeHotCredential hk) = Just hk
authHk _ = Nothing

-- | Calculates the ratio of DReps that have voted for the governance action
calculateDRepAcceptedRatio ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Rational
calculateDRepAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pure $
    dRepAcceptedRatio @(BabelEra StandardCrypto)
      ratEnv
      (gas ^. gasDRepVotesL)
      (gasAction gas)

-- | Calculates the ratio of Committee members that have voted for the governance
-- action
calculateCommitteeAcceptedRatio ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Rational
calculateCommitteeAcceptedRatio gaId = do
  eNo <- getsNES nesELL
  RatifyEnv {reCommitteeState} <- getRatifyEnv
  GovActionState {gasCommitteeVotes} <- getGovActionState gaId
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  let
    members = foldMap' (committeeMembers @(BabelEra StandardCrypto)) committee
  pure $
    committeeAcceptedRatio
      members
      gasCommitteeVotes
      reCommitteeState
      eNo

calculatePoolAcceptedRatio ::
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Rational
calculatePoolAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pure $ spoAcceptedRatio ratEnv gas

-- | Logs the ratios of accepted votes per category
logAcceptedRatio ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
logAcceptedRatio aId = do
  dRepRatio <- calculateDRepAcceptedRatio aId
  committeeRatio <- calculateCommitteeAcceptedRatio aId
  spoRatio <- calculatePoolAcceptedRatio aId
  logEntry $
    unlines
      [ ""
      , "----- ACCEPTED RATIOS -----"
      , "DRep accepted ratio:\t\t" <> show dRepRatio
      , "Committee accepted ratio:\t" <> show committeeRatio
      , "SPO accepted ratio:\t\t" <> show spoRatio
      ]

getRatifyEnvAndState ::
  ImpTestM
    (BabelEra StandardCrypto)
    (RatifyEnv (BabelEra StandardCrypto), RatifyState (BabelEra StandardCrypto))
getRatifyEnvAndState = do
  ratifyEnv <- getRatifyEnv
  enactState <- getEnactState
  let ratifyState =
        RatifyState
          { rsEnactState = enactState
          , rsEnacted = mempty
          , rsExpired = mempty
          , rsDelayed = False
          }
  pure (ratifyEnv, ratifyState)

-- | Checks whether the governance action has enough DRep votes to be accepted in the next
-- epoch. (Note that no other checks except DRep votes are used)
isDRepAccepted ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Bool
isDRepAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ dRepAccepted ratifyEnv ratifyState action

isSpoAccepted ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Bool
isSpoAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ spoAccepted ratifyEnv ratifyState action

isCommitteeAccepted ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Bool
isCommitteeAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ committeeAccepted ratifyEnv ratifyState action

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks ::
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
logRatificationChecks gaId = do
  gas@GovActionState {gasCommitteeVotes, gasDRepVotes} <- getGovActionState gaId
  let govAction = gasAction gas
  ens@EnactState {..} <- getEnactState
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  ratEnv@RatifyEnv {reCurrentEpoch} <- getRatifyEnv
  let ratSt = RatifyState ens mempty mempty False
  curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  currentEpoch <- getsNES nesELL
  let
    members = foldMap' committeeMembers committee
    committeeState = reCommitteeState ratEnv
  curPParams <- getsNES $ nesEsL . epochStateGovStateL . curPParamsGovStateL
  logEntry $
    unlines
      [ "----- RATIFICATION CHECKS -----"
      , "prevActionAsExpected:\t" <> show (prevActionAsExpected gas ensPrevGovActionIds)
      , "validCommitteeTerm:\t" <> show (validCommitteeTerm govAction curPParams currentEpoch)
      , "notDelayed:\t\t??"
      , "withdrawalCanWithdraw:\t" <> show (withdrawalCanWithdraw govAction curTreasury)
      , "committeeAccepted:\t"
          <> show (committeeAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (committeeAcceptedRatio members gasCommitteeVotes committeeState currentEpoch)
          <> " >= "
          <> show (votingCommitteeThreshold reCurrentEpoch ratSt committeeState (gasAction gas))
          <> " ]"
      , "spoAccepted:\t\t"
          <> show (spoAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (spoAcceptedRatio ratEnv gas)
          <> " >= "
          <> show (votingStakePoolThreshold ratSt (gasAction gas))
          <> " ]"
      , "dRepAccepted:\t\t"
          <> show (dRepAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (dRepAcceptedRatio ratEnv gasDRepVotes (gasAction gas))
          <> " >= "
          <> show (votingDRepThreshold ratSt (gasAction gas))
          <> " ]"
      , ""
      ]

-- | Submits a transaction that registers a hot key for the given cold key.
-- Returns the hot key hash.
registerCommitteeHotKey ::
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    (Credential 'HotCommitteeRole (EraCrypto (BabelEra StandardCrypto)))
registerCommitteeHotKey coldKey = do
  hotKey <- KeyHashObj <$> freshKeyHash
  submitTxAnn_ "Registering Committee Hot key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton (AuthCommitteeHotKeyTxCert coldKey hotKey)
  pure hotKey

-- | Submits a transaction that resigns the cold key
resignCommitteeColdKey ::
  Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  StrictMaybe (Anchor (EraCrypto (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
resignCommitteeColdKey coldKey anchor = do
  submitTxAnn_ "Resigning Committee Cold key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton (ResignCommitteeColdTxCert coldKey anchor)

electCommittee ::
  HasCallStack =>
  StrictMaybe (GovPurposeId 'CommitteePurpose (BabelEra StandardCrypto)) ->
  Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto)) ->
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto))) ->
  Map.Map (Credential 'ColdCommitteeRole (EraCrypto (BabelEra StandardCrypto))) EpochNo ->
  ImpTestM (BabelEra StandardCrypto) (GovPurposeId 'CommitteePurpose (BabelEra StandardCrypto))
electCommittee prevGovId drep toRemove toAdd = impAnn "Electing committee" $ do
  let
    committeeAction =
      UpdateCommittee
        prevGovId
        toRemove
        toAdd
        (1 %! 2)
  gaidCommitteeProp <- submitGovAction committeeAction
  submitYesVote_ (DRepVoter drep) gaidCommitteeProp
  pure (GovPurposeId gaidCommitteeProp)

electBasicCommittee ::
  HasCallStack =>
  ImpTestM
    (BabelEra StandardCrypto)
    ( Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto))
    , Credential 'HotCommitteeRole (EraCrypto (BabelEra StandardCrypto))
    , GovPurposeId 'CommitteePurpose (BabelEra StandardCrypto)
    )
electBasicCommittee = do
  logEntry "Setting up a DRep"
  (drep, _, _) <- setupSingleDRep 1_000_000

  logEntry "Registering committee member"
  coldCommitteeC <- KeyHashObj <$> freshKeyHash
  let
    committeeAction =
      UpdateCommittee
        SNothing
        mempty
        (Map.singleton coldCommitteeC 20)
        (1 %! 2)
  (gaidCommitteeProp NE.:| _) <-
    submitGovActions
      [ committeeAction
      , UpdateCommittee SNothing mempty mempty (1 %! 10)
      ]
  submitYesVote_ (DRepVoter drep) gaidCommitteeProp
  passEpoch
  passEpoch
  hotCommitteeC <- registerCommitteeHotKey coldCommitteeC
  pure (drep, hotCommitteeC, GovPurposeId gaidCommitteeProp)

logCurPParams ::
  ToExpr (PParamsHKD Identity (BabelEra StandardCrypto)) =>
  ImpTestM (BabelEra StandardCrypto) ()
logCurPParams = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  logEntry $
    unlines
      [ ""
      , "----- Current PParams -----"
      , showExpr pp
      , "---------------------------"
      , ""
      ]

proposalsShowDebug :: Proposals (BabelEra StandardCrypto) -> Bool -> String
proposalsShowDebug ps showRoots =
  unlines $
    [ ""
    , "----- Proposals -----"
    , "Size"
    , show $ proposalsSize ps
    , "OMap"
    , show $ proposalsIds ps
    , ""
    , "Roots"
    , "> PParamUpdate"
    , show $ ps ^. pRootsL . grPParamUpdateL
    , "> HardFork"
    , show $ ps ^. pRootsL . grHardForkL
    , "> Committee"
    , show $ ps ^. pRootsL . grCommitteeL
    , "> Constitution"
    , show $ ps ^. pRootsL . grConstitutionL
    ]
      <> ( if showRoots
            then
              [ "Hierarchy"
              , ">> PParamUpdate"
              , show $ ps ^. pGraphL . grPParamUpdateL . pGraphNodesL
              , ">> HardFork"
              , show $ ps ^. pGraphL . grHardForkL . pGraphNodesL
              , ">> Committee"
              , show $ ps ^. pGraphL . grCommitteeL . pGraphNodesL
              , ">> Constitution"
              , show $ ps ^. pGraphL . grConstitutionL . pGraphNodesL
              ]
            else mempty
         )
      <> ["----- Proposals End -----"]

submitConstitutionGovAction ::
  StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
submitConstitutionGovAction gid = do
  constitutionHash <- freshSafeHash
  let constitutionAction =
        NewConstitution
          (GovPurposeId <$> gid)
          ( Constitution
              ( Anchor
                  (fromJust $ textToUrl 64 "constitution.dummy.0")
                  constitutionHash
              )
              SNothing
          )
  submitGovAction constitutionAction

getProposalsForest ::
  ImpTestM
    (BabelEra StandardCrypto)
    (Forest (StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto)))))
getProposalsForest = do
  ps <- getProposals
  pure
    [ Node (mkRoot grPParamUpdateL ps) $ mkForest grPParamUpdateL ps
    , Node (mkRoot grHardForkL ps) $ mkForest grHardForkL ps
    , Node (mkRoot grCommitteeL ps) $ mkForest grCommitteeL ps
    , Node (mkRoot grConstitutionL ps) $ mkForest grConstitutionL ps
    ]
  where
    mkRoot ::
      Lens'
        (GovRelation PRoot (BabelEra StandardCrypto))
        (PRoot (GovPurposeId p (BabelEra StandardCrypto))) ->
      Proposals (BabelEra StandardCrypto) ->
      StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto)))
    mkRoot rootL ps = fmap unGovPurposeId $ ps ^. pRootsL . rootL . prRootL
    mkForest ::
      ( forall f.
        Lens' (GovRelation f (BabelEra StandardCrypto)) (f (GovPurposeId p (BabelEra StandardCrypto)))
      ) ->
      Proposals (BabelEra StandardCrypto) ->
      Forest (StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))))
    mkForest forestL ps =
      let h = ps ^. pGraphL . forestL . pGraphNodesL
          s = toList $ proposalsIds ps
          getOrderedChildren cs = filter (`Set.member` Set.map unGovPurposeId cs) s
          go c = (SJust c, getOrderedChildren $ h Map.! GovPurposeId c ^. peChildrenL)
       in unfoldForest go (getOrderedChildren $ ps ^. pRootsL . forestL . prChildrenL)

submitGovActionTree ::
  ( StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
    ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
  ) ->
  StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
  Tree () ->
  ImpTestM (BabelEra StandardCrypto) (Tree (GovActionId (EraCrypto (BabelEra StandardCrypto))))
submitGovActionTree submitAction p tree =
  unfoldTreeM go $ fmap (const p) tree
  where
    go (Node parent children) = do
      n <- submitAction parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

submitGovActionForest ::
  ( StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
    ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
  ) ->
  StrictMaybe (GovActionId (EraCrypto (BabelEra StandardCrypto))) ->
  Forest () ->
  ImpTestM (BabelEra StandardCrypto) (Forest (GovActionId (EraCrypto (BabelEra StandardCrypto))))
submitGovActionForest submitAction p forest =
  unfoldForestM go $ fmap (fmap $ const p) forest
  where
    go (Node parent children) = do
      n <- submitAction parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

enactConstitution ::
  HasCallStack =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose (BabelEra StandardCrypto)) ->
  Constitution (BabelEra StandardCrypto) ->
  Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto)) ->
  Credential 'HotCommitteeRole (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (GovActionId (EraCrypto (BabelEra StandardCrypto)))
enactConstitution prevGovId constitution dRep committeeMember = impAnn "Enacting constitution" $ do
  let action = NewConstitution prevGovId constitution
  govId <- submitGovAction action
  submitYesVote_ (DRepVoter dRep) govId
  submitYesVote_ (CommitteeVoter committeeMember) govId
  logRatificationChecks govId
  passNEpochs 2
  enactedConstitution <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
  enactedConstitution `shouldBe` constitution
  pure govId

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe ::
  HasCallStack =>
  String ->
  ImpTestM (BabelEra StandardCrypto) ()
constitutionShouldBe cUrl = do
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
  anchorUrl `shouldBe` fromJust (textToUrl 64 $ T.pack cUrl)

expectNumDormantEpochs :: HasCallStack => EpochNo -> ImpTestM (BabelEra StandardCrypto) ()
expectNumDormantEpochs expected = do
  nd <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
  nd `shouldBeExpr` expected

submitConstitution ::
  StrictMaybe (GovPurposeId 'ConstitutionPurpose (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    (GovActionId (EraCrypto (BabelEra StandardCrypto)), Constitution (BabelEra StandardCrypto))
submitConstitution prevGovId = do
  constitution <- arbitrary
  let constitutionAction =
        NewConstitution
          prevGovId
          constitution
  govActionId <- submitGovAction constitutionAction
  pure (govActionId, constitution)

expectExtraDRepExpiry ::
  HasCallStack =>
  Credential 'DRepRole (EraCrypto (BabelEra StandardCrypto)) ->
  EpochNo ->
  ImpTestM (BabelEra StandardCrypto) ()
expectExtraDRepExpiry drep expected = do
  drepActivity <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppDRepActivityL
  dsMap <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  let ds = Map.lookup drep dsMap
  (^. drepExpiryL)
    <$> ds
      `shouldBe` Just (addEpochInterval expected drepActivity)

currentProposalsShouldContain ::
  HasCallStack =>
  GovActionId (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
currentProposalsShouldContain gai =
  currentProposalIds >>= flip shouldContain [gai] . toList

expectCurrentProposals ::
  HasCallStack => ImpTestM (BabelEra StandardCrypto) ()
expectCurrentProposals = do
  props <- currentProposalIds
  assertBool "Expected proposals in current gov state" (not (SSeq.null props))

expectNoCurrentProposals :: HasCallStack => ImpTestM (BabelEra StandardCrypto) ()
expectNoCurrentProposals = do
  proposals <- getProposals
  case proposalsActions proposals of
    Empty -> pure ()
    xs -> assertFailure $ "Expected no active proposals, but got:\n" <> show (toExpr xs)

expectPulserProposals ::
  HasCallStack => ImpTestM (BabelEra StandardCrypto) ()
expectPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected proposals in the pulser" (not (SSeq.null props))

expectNoPulserProposals ::
  HasCallStack => ImpTestM (BabelEra StandardCrypto) ()
expectNoPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected no proposals in the pulser" (SSeq.null props)

currentProposalIds ::
  ImpTestM
    (BabelEra StandardCrypto)
    (SSeq.StrictSeq (GovActionId (EraCrypto (BabelEra StandardCrypto))))
currentProposalIds =
  proposalsIds
    <$> getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL)

lastEpochProposals ::
  ImpTestM
    (BabelEra StandardCrypto)
    (SSeq.StrictSeq (GovActionId (EraCrypto (BabelEra StandardCrypto))))
lastEpochProposals =
  fmap (gasId @(BabelEra StandardCrypto)) . psProposals
    <$> getsNES
      ( nesEsL
          . esLStateL
          . lsUTxOStateL
          . utxosGovStateL
          . drepPulsingStateGovStateL
          . pulsingStateSnapshotL
      )

pulsingStateSnapshotL ::
  Lens' (DRepPulsingState (BabelEra StandardCrypto)) (PulsingSnapshot (BabelEra StandardCrypto))
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state

-- | A legal ProtVer that differs in the minor Version
minorFollow :: ProtVer -> ProtVer
minorFollow (ProtVer x y) = ProtVer x (y + 1)

-- | A legal ProtVer that moves to the next major Version
majorFollow :: ProtVer -> ProtVer
majorFollow pv@(ProtVer x _) = case succVersion x of
  Just x' -> ProtVer x' 0
  Nothing -> error ("The last major version can't be incremented. " ++ show pv)

-- | An illegal ProtVer that skips 3 minor versions
cantFollow :: ProtVer -> ProtVer
cantFollow (ProtVer x y) = ProtVer x (y + 3)

whenPostBootstrap ::
  ImpTestM (BabelEra StandardCrypto) () ->
  ImpTestM (BabelEra StandardCrypto) ()
whenPostBootstrap a = do
  pv <- getProtVer
  unless (HardForks.bootstrapPhase pv) a

-- | Figure out all the Byron Addresses that need witnesses as well as all of the
-- KeyHashes for Shelley Key witnesses that are required.
impWitsVKeyNeeded ::
  TxBody (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Set.Set (BootstrapAddress (EraCrypto (BabelEra StandardCrypto))) -- Byron Based Addresses
    , Set.Set (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto))) -- Shelley Based KeyHashes
    )
impWitsVKeyNeeded txBody = do
  ls <- getsNES (nesEsL . esLStateL)
  utxo <- getUTxO
  let toBootAddr txIn = do
        txOut <- txinLookup txIn utxo
        txOut ^. bootAddrTxOutF
      bootAddrs = Set.fromList $ mapMaybe toBootAddr $ Set.toList (txBody ^. spendableInputsTxBodyF)
      bootKeyHashes = Set.map (coerceKeyRole . bootstrapKeyHash) bootAddrs
      allKeyHashes =
        getWitsVKeyNeeded (ls ^. lsCertStateL) (ls ^. lsUTxOStateL . utxosUtxoL) txBody
  pure (bootAddrs, allKeyHashes Set.\\ bootKeyHashes)

iteFixupL :: Lens' (ImpTestEnv era) (Tx era -> ImpTestM era (Tx era))
iteFixupL = lens iteFixup (\x y -> x {iteFixup = y})

modifyPParams ::
  (PParams (BabelEra StandardCrypto) -> PParams (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
modifyPParams f = modifyNES $ nesEsL . curPParamsEpochStateL %~ f

logStakeDistr :: ImpTestM (BabelEra StandardCrypto) ()
logStakeDistr = do
  stakeDistr <- getsNES $ nesEsL . epochStateIncrStakeDistrL
  logEntry $ "Stake distr: " <> showExpr stakeDistr

applyParamsQCGen ::
  Params ->
  ImpTestState (BabelEra StandardCrypto) ->
  (Maybe Int, ImpTestState (BabelEra StandardCrypto))
applyParamsQCGen params impTestState =
  case replay (paramsQuickCheckArgs params) of
    Nothing -> (Nothing, impTestState)
    Just (qcGen, qcSize) -> (Just qcSize, mixinCurrentGen impTestState qcGen)

-- | Instead of reqplacing the curren QC generator in the state, we use the current and
-- the supplied to make the new generator
mixinCurrentGen ::
  ImpTestState (BabelEra StandardCrypto) -> QCGen -> ImpTestState (BabelEra StandardCrypto)
mixinCurrentGen impTestState qcGen =
  impTestState {impGen = integerVariant (fst (Random.random (impGen impTestState))) qcGen}

evalImpTestGenM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  Gen (IO b)
evalImpTestGenM impState = fmap (fmap fst) . runImpTestGenM impState

evalImpTestM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Maybe Int ->
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  IO b
evalImpTestM qc impState = fmap fst . runImpTestM qc impState

execImpTestGenM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  Gen (IO (ImpTestState (BabelEra StandardCrypto)))
execImpTestGenM impState = fmap (fmap snd) . runImpTestGenM impState

execImpTestM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Maybe Int ->
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  IO (ImpTestState (BabelEra StandardCrypto))
execImpTestM qcSize impState = fmap snd . runImpTestM qcSize impState

runImpTestGenM_ ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  Gen (IO ())
runImpTestGenM_ impState = fmap void . runImpTestGenM impState

runImpTestM_ ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Maybe Int ->
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  IO ()
runImpTestM_ qcSize impState = void . runImpTestM qcSize impState

runImpTestGenM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  Gen (IO (b, ImpTestState (BabelEra StandardCrypto)))
runImpTestGenM impState m =
  MkGen $ \qcGen qcSz -> runImpTestM (Just qcSz) (mixinCurrentGen impState qcGen) m

runImpTestM ::
  DSIGN StandardCrypto ~ Ed25519DSIGN =>
  Maybe Int ->
  ImpTestState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) b ->
  IO (b, ImpTestState (BabelEra StandardCrypto))
runImpTestM mQCSize impState (ImpTestM m) = do
  let qcSize = fromMaybe 30 mQCSize
  ioRef <- newIORef impState
  let
    env =
      ImpTestEnv
        { iteState = ioRef
        , iteFixup = fxupTx
        , iteQuickCheckSize = qcSize
        }
  res <-
    runReaderT m env `catchAny` \e -> do
      logsDoc <- impLog <$> readIORef ioRef
      let logs = renderString (layoutPretty defaultLayoutOptions logsDoc)
          adjustHUnitExc header (HUnitFailure srcLoc failReason) =
            toException $
              HUnitFailure srcLoc $
                case failReason of
                  Reason msg -> Reason $ logs <> "\n" <> header <> msg
                  ExpectedButGot Nothing expected got ->
                    ExpectedButGot (Just $ logs <> header) expected got
                  ExpectedButGot (Just msg) expected got ->
                    ExpectedButGot (Just (logs <> "\n" <> header <> msg)) expected got
          newExc
            | Just hUnitExc <- fromException e =
                adjustHUnitExc [] hUnitExc
            | Just (ImpException ann excThrown) <- fromException e =
                let header = unlines $ zipWith (\n str -> replicate n ' ' <> str) [0, 2 ..] ann
                 in case fromException excThrown of
                      Nothing -> toException $ ImpException [logs, header] excThrown
                      Just hUnitExc -> adjustHUnitExc header hUnitExc
            | otherwise = toException $ ImpException [logs] e
      throwIO newExc
  endState <- readIORef ioRef
  pure (res, endState)

runShelleyBase :: Globals -> ShelleyBase a -> a
runShelleyBase globals act = runIdentity $ runReaderT act globals

getRewardAccountAmount ::
  RewardAccount (EraCrypto (BabelEra StandardCrypto)) -> ImpTestM (BabelEra StandardCrypto) Coin
getRewardAccountAmount rewardAcount = do
  umap <- getsNES $ nesEsL . epochStateUMapL
  let cred = raCredential rewardAcount
  case UMap.lookup cred (RewDepUView umap) of
    Nothing -> assertFailure $ "Expected a reward account: " ++ show cred
    Just RDPair {rdReward} -> pure $ fromCompact rdReward

lookupImpRootTxOut ::
  ImpTestM
    (BabelEra StandardCrypto)
    (TxIn (EraCrypto (BabelEra StandardCrypto)), TxOut (BabelEra StandardCrypto))
lookupImpRootTxOut = do
  ImpTestState {impRootTxIn} <- get
  utxo <- getUTxO
  case txinLookup impRootTxIn utxo of
    Nothing -> error "Root txId no longer points to an existing unspent output"
    Just rootTxOut -> pure (impRootTxIn, rootTxOut)

impAddNativeScript ::
  NativeScript (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (ScriptHash (EraCrypto (BabelEra StandardCrypto)))
impAddNativeScript nativeScript = do
  let script = fromNativeScript nativeScript
      scriptHash = hashScript @(BabelEra StandardCrypto) script
  impNativeScriptsL %= Map.insert scriptHash nativeScript
  pure scriptHash

impLogL :: Lens' (ImpTestState era) (Doc ())
impLogL = lens impLog (\x y -> x {impLog = y})

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

impLastTickL :: Lens' (ImpTestState era) SlotNo
impLastTickL = lens impLastTick (\x y -> x {impLastTick = y})

impLastTickG :: SimpleGetter (ImpTestState era) SlotNo
impLastTickG = impLastTickL

impRootTxInL :: Lens' (ImpTestState era) (TxIn (EraCrypto era))
impRootTxInL = lens impRootTxIn (\x y -> x {impRootTxIn = y})

impKeyPairsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
impKeyPairsG = to impKeyPairs

impNativeScriptsL ::
  Lens'
    (ImpTestState era)
    (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsL = lens impNativeScripts (\x y -> x {impNativeScripts = y})

impNativeScriptsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsG = impNativeScriptsL

impEventsL :: Lens' (ImpTestState era) [SomeSTSEvent era]
impEventsL = lens impEvents (\x y -> x {impEvents = y})

impNativeScriptsRequired ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    (Map (ScriptHash (EraCrypto (BabelEra StandardCrypto))) (NativeScript (BabelEra StandardCrypto)))
impNativeScriptsRequired tx = do
  utxo <- getUTxO
  ImpTestState {impNativeScripts} <- get
  let needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      hashesNeeded = getScriptsHashesNeeded needed
  pure $ impNativeScripts `Map.restrictKeys` hashesNeeded

-- | Modifies transaction by adding necessary scripts
addNativeScriptTxWits ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
addNativeScriptTxWits tx = impAnn "addNativeScriptTxWits" $ do
  scriptsRequired <- impNativeScriptsRequired tx
  utxo <- getUTxO
  let ScriptsProvided provided = getScriptsProvided utxo tx
      scriptsToAdd = scriptsRequired Map.\\ provided
  pure $
    tx
      & witsTxL
      . scriptTxWitsL
      <>~ fmap fromNativeScript scriptsToAdd

-- | Adds @TxWits@ that will satisfy all of the required key witnesses
updateAddrTxWits ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
updateAddrTxWits tx = impAnn "updateAddrTxWits" $ do
  let txBody = tx ^. bodyTxL
      txBodyHash = hashAnnotated txBody
  (bootAddrs, witsVKeyNeeded) <- impWitsVKeyNeeded txBody
  -- Shelley Based Addr Witnesses
  let curAddrWitHashes = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  extraKeyPairs <-
    mapM
      lookupKeyPair
      $ Set.toList (witsVKeyNeeded Set.\\ curAddrWitHashes)
  let extraAddrVKeyWits = mkWitnessesVKey txBodyHash extraKeyPairs
      addrWitHashes = curAddrWitHashes <> Set.map witVKeyHash extraAddrVKeyWits
  -- Shelley Based Native Script Witnesses
  scriptsRequired <- impNativeScriptsRequired tx
  nativeScriptsKeyPairs <- mapM (satisfyNativeScript addrWitHashes) (Map.elems scriptsRequired)
  let extraNativeScriptVKeyWits =
        mkWitnessesVKey txBodyHash $ Map.elems (mconcat (catMaybes nativeScriptsKeyPairs))
  -- Byron Based Witessed
  let curBootAddrWitHashes = Set.map bootstrapWitKeyHash $ tx ^. witsTxL . bootAddrTxWitsL
      bootAddrWitsNeeded =
        [ bootAddr
        | bootAddr <- Set.toList bootAddrs
        , not (coerceKeyRole (bootstrapKeyHash bootAddr) `Set.member` curBootAddrWitHashes)
        ]
  extraBootAddrWits <- forM bootAddrWitsNeeded $ \bootAddr@(BootstrapAddress byronAddr) -> do
    ByronKeyPair _ signingKey <- lookupByronKeyPair bootAddr
    let attrs = Byron.addrAttributes byronAddr
    pure $ makeBootstrapWitness (extractHash txBodyHash) signingKey attrs
  pure $
    tx
      & witsTxL
      . addrTxWitsL
      <>~ extraAddrVKeyWits
      <> extraNativeScriptVKeyWits
        & witsTxL
        . bootAddrTxWitsL
        <>~ Set.fromList extraBootAddrWits

-- | This fixup step ensures that there are enough funds in the transaction.
addRootTxIn ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
addRootTxIn tx = impAnn "addRootTxIn" $ do
  rootTxIn <- fst <$> lookupImpRootTxOut
  pure $
    tx
      & bodyTxL
      . inputsTxBodyL
      %~ Set.insert rootTxIn

impNativeScriptKeyPairs ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Map
        (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto)))
        (KeyPair 'Witness (EraCrypto (BabelEra StandardCrypto)))
    )
impNativeScriptKeyPairs tx = do
  scriptsRequired <- impNativeScriptsRequired tx
  let nativeScripts = Map.elems scriptsRequired
      curAddrWits = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  keyPairs <- mapM (satisfyNativeScript curAddrWits) nativeScripts
  pure . mconcat $ catMaybes keyPairs

satisfyNativeScript ::
  Set.Set (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto))) ->
  NativeScript (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Maybe
        ( Map.Map
            (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto)))
            (KeyPair 'Witness (EraCrypto (BabelEra StandardCrypto)))
        )
    )
satisfyNativeScript providedVKeyHashes script = do
  impState <- get
  let
    keyPairs = impState ^. impKeyPairsG
    prevSlotNo = impState ^. impLastTickG
    satisfyMOf m Empty
      | m <= 0 = Just mempty
      | otherwise = Nothing
    satisfyMOf m (x :<| xs) =
      case satisfyScript x of
        Nothing -> satisfyMOf m xs
        Just kps -> do
          kps' <- satisfyMOf (m - 1) xs
          Just $ kps <> kps'
    satisfyScript = \case
      RequireSignature keyHash
        | keyHash `Set.member` providedVKeyHashes -> Just mempty
        | otherwise -> do
            keyPair <- Map.lookup keyHash keyPairs
            Just $ Map.singleton keyHash keyPair
      RequireAllOf ss -> satisfyMOf (length ss) ss
      RequireAnyOf ss -> satisfyMOf 1 ss
      RequireMOf m ss -> satisfyMOf m ss
      RequireTimeExpire slotNo
        | slotNo < prevSlotNo -> Just mempty
        | otherwise -> Nothing
      RequireTimeStart slotNo
        | slotNo > prevSlotNo -> Just mempty
        | otherwise -> Nothing
  pure $ satisfyScript script

-- satisfyNativeScript ::
--
--   ( c ~ EraCrypto (BabelEra StandardCrypto)
--
--   , NFData (SigDSIGN (DSIGN StandardCrypto))
--   , NFData (VerKeyDSIGN (DSIGN StandardCrypto))
--   , Signable (DSIGN StandardCrypto) (Hash (HASH StandardCrypto) EraIndependentTxBody)
--   ) =>
--   Set.Set (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto))) ->
--   NativeScript (BabelEra StandardCrypto) ->
--   ImpTestM
--     (BabelEra StandardCrypto)
--     ( Maybe (Map (KeyHash 'Witness (EraCrypto (BabelEra StandardCrypto))) (KeyPair 'Witness (EraCrypto (BabelEra StandardCrypto))))
--     )
-- satisfyNativeScript providedVKeyHashes script = do
--   keyPairs <- gets impKeyPairs
--   let
--     satisfyMOf m []
--       | m <= 0 = Just mempty
--       | otherwise = Nothing
--     satisfyMOf m (x : xs) =
--       case satisfyScript x of
--         Nothing -> satisfyMOf m xs
--         Just kps -> do
--           kps' <- satisfyMOf (m - 1) xs
--           Just $ kps <> kps'
--     satisfyScript :: Timelock (BabelEra StandardCrypto) -> Maybe (Map (KeyHash 'Witness c) (KeyPair 'Witness c))
--     satisfyScript = \case
--       RequireSignature keyHash
--         | keyHash `Set.member` providedVKeyHashes -> Just mempty
--         | otherwise -> do
--             keyPair <- Map.lookup keyHash keyPairs
--             Just $ Map.singleton keyHash keyPair
--       RequireAllOf ss -> satisfyMOf (length ss) ss
--       RequireAnyOf ss -> satisfyMOf 1 ss
--       RequireMOf m ss -> satisfyMOf m ss
--   pure $ satisfyScript script

fixupFees ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
fixupFees tx = impAnn "fixupFees" $ do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  (_, kpSpending) <- freshKeyPair
  (_, kpStaking) <- freshKeyPair
  nativeScriptKeyPairs <- impNativeScriptKeyPairs tx
  let
    nativeScriptKeyWits = Map.keysSet nativeScriptKeyPairs
    consumedValue = consumed pp certState utxo (tx ^. bodyTxL)
    producedValue = produced pp certState (tx ^. bodyTxL)
    ensureNonNegativeCoin v
      | pointwise (<=) zero v = pure v
      | otherwise = do
          logEntry $ "Failed to validate coin: " <> show v
          pure zero
  logEntry "Validating changeBeforeFee"
  changeBeforeFee <- ensureNonNegativeCoin $ coin consumedValue <-> coin producedValue
  logToExpr changeBeforeFee
  let
    changeBeforeFeeTxOut =
      mkBasicTxOut
        (mkAddr (kpSpending, kpStaking))
        (inject changeBeforeFee)
    txNoWits = tx & bodyTxL . outputsTxBodyL %~ (:|> changeBeforeFeeTxOut)
    outsBeforeFee = tx ^. bodyTxL . outputsTxBodyL
    fee = calcMinFeeTxNativeScriptWits utxo pp txNoWits nativeScriptKeyWits
  logEntry "Validating change"
  change <- ensureNonNegativeCoin $ changeBeforeFeeTxOut ^. coinTxOutL <-> fee
  logToExpr change
  let
    changeTxOut = changeBeforeFeeTxOut & coinTxOutL .~ change
    -- If the remainder is sufficently big we add it to outputs, otherwise we add the
    -- extraneous coin to the fee and discard the remainder TxOut
    txWithFee
      | change >= getMinCoinTxOut pp changeTxOut =
          txNoWits
            & bodyTxL
            . outputsTxBodyL
            .~ (outsBeforeFee :|> changeTxOut)
            & bodyTxL
            . feeTxBodyL
            .~ fee
      | otherwise =
          txNoWits
            & bodyTxL
            . outputsTxBodyL
            .~ outsBeforeFee
            & bodyTxL
            . feeTxBodyL
            .~ (fee <> change)
  pure txWithFee

logFeeMismatch ::
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
logFeeMismatch tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let Coin feeUsed = tx ^. bodyTxL . feeTxBodyL
      Coin feeMin = getMinFeeTxUtxo pp tx utxo
  when (feeUsed /= feeMin) $ do
    logEntry $
      "Estimated fee " <> show feeUsed <> " while required fee is " <> show feeMin

submitTx_ ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTx_ = void . submitTx

submitTx ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
submitTx tx = trySubmitTx tx >>= expectRightDeepExpr

trySubmitTx ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (Tx (BabelEra StandardCrypto))
    )
trySubmitTx tx = do
  txFixed <- asks iteFixup >>= ($ tx)
  logToExpr txFixed
  st <- gets impNES
  lEnv <- impLedgerEnv st
  ImpTestState {impRootTxIn} <- get
  res <- tryRunImpRule @"LEDGER" lEnv (st ^. nesEsL . esLStateL) txFixed
  case res of
    Left predFailures -> do
      -- Verify that produced predicate failures are ready for the node-to-client protocol
      liftIO $ forM_ predFailures $ roundTripEraExpectation @(BabelEra StandardCrypto)
      pure $ Left predFailures
    Right (st', events) -> do
      let txId = TxId . hashAnnotated $ txFixed ^. bodyTxL
          outsSize = SSeq.length $ txFixed ^. bodyTxL . outputsTxBodyL
          rootIndex
            | outsSize > 0 = outsSize - 1
            | otherwise = error ("Expected at least 1 output after submitting tx: " <> show txId)
      tell $ fmap (SomeSTSEvent @(BabelEra StandardCrypto) @"LEDGER") events
      modify $ impNESL . nesEsL . esLStateL .~ st'
      UTxO utxo <- getUTxO
      -- This TxIn is in the utxo, and thus can be the new root, only if the transaction
      -- was phase2-valid.  Otherwise, no utxo with this id would have been created, and
      -- so we need to set the new root to what it was before the submission.
      let assumedNewRoot = TxIn txId (mkTxIxPartial (fromIntegral rootIndex))
      let newRoot
            | Map.member assumedNewRoot utxo = assumedNewRoot
            | Map.member impRootTxIn utxo = impRootTxIn
            | otherwise = error "Root not found in UTxO"
      impRootTxInL .= newRoot
      pure $ Right txFixed

impLedgerEnv ::
  NewEpochState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (LedgerEnv (BabelEra StandardCrypto))
impLedgerEnv nes = do
  slotNo <- gets impLastTick
  pure
    LedgerEnv
      { ledgerSlotNo = slotNo
      , ledgerPp = nes ^. nesEsL . curPParamsEpochStateL
      , ledgerIx = TxIx 0
      , ledgerAccount = nes ^. nesEsL . esAccountStateL
      }

-- | Submit a transaction that is expected to be rejected. The inputs and
-- outputs are automatically balanced.
submitFailingTx ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingTx tx expectedFailure = trySubmitTx tx >>= (`shouldBeLeftExpr` expectedFailure)

tryRunImpRule ::
  forall rule.
  ( STS (EraRule rule (BabelEra StandardCrypto))
  , BaseM (EraRule rule (BabelEra StandardCrypto)) ~ ShelleyBase
  ) =>
  Environment (EraRule rule (BabelEra StandardCrypto)) ->
  State (EraRule rule (BabelEra StandardCrypto)) ->
  Signal (EraRule rule (BabelEra StandardCrypto)) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (NonEmpty (PredicateFailure (EraRule rule (BabelEra StandardCrypto))))
        (State (EraRule rule (BabelEra StandardCrypto)), [Event (EraRule rule (BabelEra StandardCrypto))])
    )
tryRunImpRule stsEnv stsState stsSignal = do
  let trc = TRC (stsEnv, stsState, stsSignal)
  globals <- use $ to impGlobals
  let
    stsOpts =
      ApplySTSOpts
        { asoValidation = ValidateAll
        , asoEvents = EPReturn
        , asoAssertions = AssertionsAll
        }
  pure $
    runShelleyBase globals (applySTSOptsEither @(EraRule rule (BabelEra StandardCrypto)) stsOpts trc)

runImpRule ::
  forall rule.
  ( HasCallStack
  , KnownSymbol rule
  , STS (EraRule rule (BabelEra StandardCrypto))
  , BaseM (EraRule rule (BabelEra StandardCrypto)) ~ ShelleyBase
  , NFData (State (EraRule rule (BabelEra StandardCrypto)))
  , NFData (Event (EraRule rule (BabelEra StandardCrypto)))
  , ToExpr (Event (EraRule rule (BabelEra StandardCrypto)))
  , Eq (Event (EraRule rule (BabelEra StandardCrypto)))
  , Typeable (Event (EraRule rule (BabelEra StandardCrypto)))
  ) =>
  Environment (EraRule rule (BabelEra StandardCrypto)) ->
  State (EraRule rule (BabelEra StandardCrypto)) ->
  Signal (EraRule rule (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (State (EraRule rule (BabelEra StandardCrypto)))
runImpRule stsEnv stsState stsSignal = do
  let ruleName = symbolVal (Proxy @rule)
  (res, ev) <-
    tryRunImpRule @rule stsEnv stsState stsSignal >>= \case
      Left fs ->
        assertFailure $
          unlines $
            ("Failed to run " <> ruleName <> ":") : map show (toList fs)
      Right res -> evaluateDeep res
  tell $ fmap (SomeSTSEvent @(BabelEra StandardCrypto) @rule) ev
  pure res

-- | Runs the TICK rule once
passTick ::
  HasCallStack =>
  ImpTestM (BabelEra StandardCrypto) ()
passTick = do
  impLastTick <- gets impLastTick
  curNES <- getsNES id
  nes <- runImpRule @"TICK" () curNES impLastTick
  impLastTickL += 1
  impNESL .= nes

-- | Runs the TICK rule until the next epoch is reached
passEpoch ::
  ImpTestM (BabelEra StandardCrypto) ()
passEpoch = do
  startEpoch <- getsNES nesELL
  logEntry $ "Entering " <> show (succ startEpoch)
  let
    tickUntilNewEpoch curEpoch = do
      passTick
      newEpoch <- getsNES nesELL
      unless (newEpoch > curEpoch) $ tickUntilNewEpoch newEpoch
  preNES <- gets impNES

  tickUntilNewEpoch startEpoch
  gets impNES >>= epochBoundaryCheck preNES

epochBoundaryCheck ::
  NewEpochState (BabelEra StandardCrypto) ->
  NewEpochState (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
epochBoundaryCheck preNES postNES = do
  impAnn "Checking ADA preservation at the epoch boundary" $ do
    let preSum = tot preNES
        postSum = tot postNES
    logEntry $ diffExpr preSum postSum
    unless (preSum == postSum) . expectationFailure $
      "Total ADA in the epoch state is not preserved\n\tpost - pre = "
        <> show (postSum <-> preSum)
  where
    tot nes =
      (<+>)
        (sumAdaPots (totalAdaPotsES (nes ^. nesEsL)))
        (nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosDonationL)

-- | Runs the TICK rule until the `n` epochs are passed
passNEpochs ::
  Natural ->
  ImpTestM (BabelEra StandardCrypto) ()
passNEpochs n = when (n > 0) $ passEpoch >> passNEpochs (n - 1)

-- | Stores extra information about the failure of the unit test
data ImpException = ImpException
  { ieAnnotation :: [String]
  -- ^ Description of the IO action that caused the failure
  , ieThrownException :: SomeException
  -- ^ Exception that caused the test to fail
  }

instance Show ImpException where
  show (ImpException ann e) =
    "Log:\n"
      <> unlines ann
      <> "\nFailed with Exception:\n\t"
      <> displayException e
instance Exception ImpException

-- | Annotation for when failure happens. All the logging done within annotation will be
-- discarded if there no failures within the annotation.
impAnn ::
  NFData a => String -> ImpTestM (BabelEra StandardCrypto) a -> ImpTestM (BabelEra StandardCrypto) a
impAnn msg m = do
  logs <- use impLogL
  res <- catchAnyDeep m $ \exc ->
    throwIO $
      case fromException exc of
        Just (ImpException ann origExc) -> ImpException (msg : ann) origExc
        Nothing -> ImpException [msg] exc
  impLogL .= logs
  pure res

-- | Adds a string to the log, which is only shown if the test fails
logEntry :: HasCallStack => String -> ImpTestM (BabelEra StandardCrypto) ()
logEntry e = impLogL %= (<> pretty loc <> "\t" <> pretty e <> line)
  where
    formatSrcLoc srcLoc =
      "[" <> srcLocModule srcLoc <> ":" <> show (srcLocStartLine srcLoc) <> "]\n"
    loc =
      case getCallStack ?callStack of
        (_, srcLoc) : _ -> formatSrcLoc srcLoc
        _ -> ""

logToExpr :: (HasCallStack, ToExpr a) => a -> ImpTestM (BabelEra StandardCrypto) ()
logToExpr e = logEntry (showExpr e)

withImpState ::
  SpecWith (ImpTestState (BabelEra StandardCrypto)) ->
  Spec
withImpState = withImpStateModified id

initLedgerState :: LedgerState (BabelEra StandardCrypto)
initLedgerState =
  LedgerState
    { lsUTxOState =
        smartUTxOState
          emptyPParams
          mempty
          zero
          zero
          emptyGovState
          mempty
    , lsCertState = def
    }

genesisCoins ::
  TxId (EraCrypto era) ->
  [TxOut era] ->
  UTxO era
genesisCoins genesisTxId outs =
  UTxO $
    Map.fromList [(TxIn genesisTxId idx, out) | (idx, out) <- zip [minBound ..] outs]

-- initUTxO :: UTxOState (BabelEra StandardCrypto)
-- initUTxO =
--   UTxOState
--     mempty
--     mempty
--     (Coin 0)
--     (Coin 0)
--     def
--     mempty
--     mempty

withImpStateModified ::
  (ImpTestState (BabelEra StandardCrypto) -> ImpTestState (BabelEra StandardCrypto)) ->
  SpecWith (ImpTestState (BabelEra StandardCrypto)) ->
  Spec
withImpStateModified f =
  beforeAll $
    execImpTestM Nothing (f impTestState0) $
      addRootTxOut >> newImpTestState
  where
    impTestState0 =
      ImpTestState
        { impNES = initBabelImpNES initLedgerState
        , impRootTxIn = rootTxIn
        , impKeyPairs = mempty
        , impByronKeyPairs = mempty
        , impNativeScripts = mempty
        , impLastTick = 0
        , impGlobals = testGlobals
        , impLog = mempty
        , impGen = mkQCGen 2024
        , impEvents = mempty
        }
    rootCoin = Coin 1_000_000_000
    rootTxIn = TxIn (mkTxId 0) minBound
    addRootTxOut = do
      (rootKeyHash, _) <- freshKeyPair
      let rootAddr = Addr Testnet (KeyHashObj rootKeyHash) StakeRefNull
          rootTxOut = mkBasicTxOut rootAddr $ MaryValue rootCoin totalSupply
      impNESL
        . nesEsL
        . esLStateL
        . lsUTxOStateL
        . utxosUtxoL
        %= (<> UTxO (Map.singleton rootTxIn rootTxOut))

-- This is the most lax policy possible, requiring no authorization at all.
purplePolicy :: Timelock Mary
purplePolicy = RequireAllOf (SSeq.fromList [])

purplePolicyId :: PolicyID StandardCrypto
purplePolicyId = PolicyID $ hashScript @Mary purplePolicy

plum :: AssetName
plum = AssetName "plum"

amethyst :: AssetName
amethyst = AssetName "amethyst"

totalSupply :: MultiAsset StandardCrypto
totalSupply =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(amethyst, 100)])

smallValue :: MultiAsset StandardCrypto
smallValue =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(amethyst, 10)])

mediumValue :: MultiAsset StandardCrypto
mediumValue =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(amethyst, 20)])

largeValue :: MultiAsset StandardCrypto
largeValue =
  MultiAsset $
    Map.singleton purplePolicyId (Map.fromList [(amethyst, 80)])

testKeyHash :: KeyHash kd StandardCrypto
testKeyHash = mkKeyHash (-1)

initBabelImpNES ::
  LedgerState (BabelEra StandardCrypto) ->
  NewEpochState (BabelEra StandardCrypto)
initBabelImpNES ls =
  NewEpochState
    { stashedAVVMAddresses = def
    , nesRu =
        SJust $
          startStep
            (EpochSize 432_000)
            (BlocksMade (Map.singleton testKeyHash 10))
            epochState
            (Coin 45)
            (activeSlotCoeff testGlobals)
            10
    , nesPd =
        PoolDistr $
          Map.fromList
            [
              ( testKeyHash
              , IndividualPoolStake
                  1
                  (mkHashVerKeyVRF 0)
              )
            ]
    , nesEs = epochState
    , nesEL = 0
    , nesBprev = BlocksMade (Map.singleton testKeyHash 10)
    , nesBcur = BlocksMade mempty
    }
  where
    pp =
      emptyPParams
        & ppMinFeeAL
        .~ Coin 44
        & ppMinFeeBL
        .~ Coin 155_381
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { asTreasury = Coin 10_000
              , asReserves = Coin 1_000
              }
        , esSnapshots = emptySnapShots
        , esLState = ls
        , esNonMyopic = def
        }
        & prevPParamsEpochStateL
        .~ pp
        & curPParamsEpochStateL
        .~ pp

mkHashVerKeyVRF ::
  Integer ->
  Hash (HASH (EraCrypto (BabelEra StandardCrypto))) (VerKeyVRF (EraCrypto (BabelEra StandardCrypto)))
mkHashVerKeyVRF =
  VRF.hashVerKeyVRF
    . VRF.deriveVerKeyVRF
    . VRF.genKeyVRF
    . mkSeedFromBytes
    . integralToByteStringN seedSize
  where
    seedSize = fromIntegral . seedSizeDSIGN $ Proxy @(DSIGN (EraCrypto (BabelEra StandardCrypto)))

mkTxId :: Int -> TxId StandardCrypto
mkTxId idx = TxId (mkDummySafeHash Proxy idx)

initImpNES ::
  NewEpochState (BabelEra StandardCrypto) ->
  NewEpochState (BabelEra StandardCrypto)
initImpNES = nesEsL . curPParamsEpochStateL %~ initPParams
  where
    initPParams pp =
      pp
        & ppMaxValSizeL
        .~ 1_000_000_000
        & ppMaxTxExUnitsL
        .~ ExUnits 10_000_000 10_000_000
        & ppCostModelsL
        .~ testingCostModels
          [PlutusV1 .. eraMaxLanguage @(BabelEra StandardCrypto)]

newImpTestState ::
  forall m.
  ( GovState (BabelEra StandardCrypto) ~ ConwayGovState (BabelEra StandardCrypto)
  , MonadState (ImpTestState (BabelEra StandardCrypto)) m
  , MonadGen m
  ) =>
  m ()
newImpTestState = do
  kh <- fst <$> freshKeyPair
  let committee = Committee [(KeyHashObj kh, EpochNo 15)] (1 %! 1)
  anchor <- arbitrary
  let constitution = Constitution anchor SNothing
  impNESL %= initBabelNES committee constitution
  where
    initBabelNES ::
      Committee (BabelEra StandardCrypto) ->
      Constitution (BabelEra StandardCrypto) ->
      NewEpochState (BabelEra StandardCrypto) ->
      NewEpochState (BabelEra StandardCrypto)
    initBabelNES committee constitution nes =
      let newNes =
            initImpNES nes
              & nesEsL
              . curPParamsEpochStateL
              . ppDRepActivityL
              .~ EpochInterval 100
              & nesEsL
              . curPParamsEpochStateL
              . ppGovActionLifetimeL
              .~ EpochInterval 30
              & nesEsL
              . curPParamsEpochStateL
              . ppGovActionDepositL
              .~ Coin 123
              & nesEsL
              . curPParamsEpochStateL
              . ppCommitteeMaxTermLengthL
              .~ EpochInterval 20
              & nesEsL
              . curPParamsEpochStateL
              . ppCommitteeMinSizeL
              .~ 1
              & nesEsL
              . curPParamsEpochStateL
              . ppDRepVotingThresholdsL
              %~ ( \dvt ->
                    dvt
                      { dvtCommitteeNormal = 1 %! 1
                      , dvtCommitteeNoConfidence = 1 %! 2
                      , dvtUpdateToConstitution = 1 %! 2
                      }
                 )
              & nesEsL
              . epochStateGovStateL
              . committeeGovStateL
              .~ SJust committee
              & nesEsL
              . epochStateGovStateL
              . constitutionGovStateL
              .~ constitution
          epochState = newNes ^. nesEsL
          ratifyState =
            def
              & rsEnactStateL
              .~ mkEnactState (epochState ^. epochStateGovStateL)
       in newNes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

-- | Creates a fresh @SafeHash@
freshSafeHash ::
  ImpTestM (BabelEra StandardCrypto) (SafeHash (EraCrypto (BabelEra StandardCrypto)) a)
freshSafeHash = arbitrary

freshKeyHashVRF ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( Hash (HASH (EraCrypto (BabelEra StandardCrypto))) (VerKeyVRF (EraCrypto (BabelEra StandardCrypto)))
    )
freshKeyHashVRF = arbitrary

-- | Adds a key pair to the keyhash lookup map
addKeyPair ::
  MonadState (ImpTestState (BabelEra StandardCrypto)) m =>
  KeyPair r (EraCrypto (BabelEra StandardCrypto)) ->
  m (KeyHash r (EraCrypto (BabelEra StandardCrypto)))
addKeyPair keyPair@(KeyPair vk _) = do
  ImpTestState {impKeyPairs} <- get
  let keyHash = hashKey vk
  modify $ \st ->
    st
      { impKeyPairs =
          Map.insert
            (coerceKeyRole keyHash)
            (coerce keyPair)
            impKeyPairs
      }
  pure keyHash

-- | Looks up the `KeyPair` corresponding to the `KeyHash`. The `KeyHash` must be
-- created with `freshKeyHash` for this to work.
lookupKeyPair ::
  HasCallStack =>
  KeyHash r (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (KeyPair r (EraCrypto (BabelEra StandardCrypto)))
lookupKeyPair keyHash = do
  keyPairs <- gets impKeyPairs
  case Map.lookup keyHash keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show keyHash
          ++ "\nAlways use `freshKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `KeyPair` in the
-- ImpTestState. If you also need the `KeyPair` consider using `freshKeyPair` for
-- generation or `lookupKeyPair` to look up the `KeyPair` corresponding to the `KeyHash`
freshKeyHash ::
  ImpTestM (BabelEra StandardCrypto) (KeyHash r (EraCrypto (BabelEra StandardCrypto)))
freshKeyHash = fst <$> freshKeyPair

-- | Generate a random KeyPair and add it to the known keys in the Imp state
freshKeyPair ::
  (MonadState (ImpTestState (BabelEra StandardCrypto)) m, MonadGen m) =>
  m (KeyHash r (EraCrypto (BabelEra StandardCrypto)), KeyPair r (EraCrypto (BabelEra StandardCrypto)))
freshKeyPair = do
  keyPair <- arbitrary
  keyHash <- addKeyPair keyPair
  pure (keyHash, keyPair)

freshKeyAddr ::
  ImpTestM
    (BabelEra StandardCrypto)
    (KeyHash r (EraCrypto (BabelEra StandardCrypto)), Addr (EraCrypto (BabelEra StandardCrypto)))
freshKeyAddr = do
  keyHash <- freshKeyHash
  pure (coerceKeyRole keyHash, Addr Testnet (KeyHashObj keyHash) StakeRefNull)

-- | Looks up the keypair corresponding to the `BootstrapAddress`. The `BootstrapAddress`
-- must be created with `freshBootstrapAddess` for this to work.
lookupByronKeyPair ::
  HasCallStack =>
  BootstrapAddress (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ByronKeyPair
lookupByronKeyPair bootAddr = do
  keyPairs <- gets impByronKeyPairs
  case Map.lookup bootAddr keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show bootAddr
          ++ "\nAlways use `freshByronKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `ByronKeyPair` in the
-- ImpTestState. If you also need the `ByronKeyPair` consider using `freshByronKeyPair` for
-- generation or `lookupByronKeyPair` to look up the `ByronKeyPair` corresponding to the `KeyHash`
freshByronKeyHash ::
  ImpTestM (BabelEra StandardCrypto) (KeyHash r (EraCrypto (BabelEra StandardCrypto)))
freshByronKeyHash = coerceKeyRole . bootstrapKeyHash <$> freshBootstapAddress

freshBootstapAddress ::
  ImpTestM (BabelEra StandardCrypto) (BootstrapAddress (EraCrypto (BabelEra StandardCrypto)))
freshBootstapAddress = do
  ImpTestState {impByronKeyPairs} <- get
  keyPair@(ByronKeyPair verificationKey _) <- arbitrary
  payload <-
    oneof
      [ pure Nothing
      , Just . Byron.HDAddressPayload <$> (uniformByteStringM =<< choose (0, 63))
      ]
  let asd = Byron.VerKeyASD verificationKey
      attrs = Byron.AddrAttributes payload (Byron.NetworkTestnet 0)
      bootAddr = BootstrapAddress $ Byron.makeAddress asd attrs
  modify $ \st ->
    st {impByronKeyPairs = Map.insert bootAddr keyPair impByronKeyPairs}
  pure bootAddr

sendCoinTo ::
  HasCallStack =>
  Addr (EraCrypto (BabelEra StandardCrypto)) ->
  Coin ->
  ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
sendCoinTo addr = sendValueTo addr . inject

sendValueTo ::
  HasCallStack =>
  Addr (EraCrypto (BabelEra StandardCrypto)) ->
  Value (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
sendValueTo addr amount = do
  tx <-
    submitTxAnn
      ("Giving " <> show amount <> " to " <> show addr)
      $ mkBasicTx mkBasicTxBody
        & bodyTxL
        . outputsTxBodyL
        .~ SSeq.singleton (mkBasicTxOut addr amount)
  pure $ txInAt (0 :: Int) tx

-- | Modify the current new epoch state with a function
modifyNES ::
  (NewEpochState (BabelEra StandardCrypto) -> NewEpochState (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
modifyNES = (impNESL %=)

-- | Get a value from the current new epoch state using the lens
getsNES ::
  SimpleGetter (NewEpochState (BabelEra StandardCrypto)) a -> ImpTestM (BabelEra StandardCrypto) a
getsNES l = gets . view $ impNESL . l

getUTxO :: ImpTestM (BabelEra StandardCrypto) (UTxO (BabelEra StandardCrypto))
getUTxO = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL

getProtVer :: ImpTestM (BabelEra StandardCrypto) ProtVer
getProtVer = getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

submitTxAnn ::
  HasCallStack =>
  String ->
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
submitTxAnn msg tx = impAnn msg (trySubmitTx tx >>= expectRightDeepExpr)

submitTxAnn_ ::
  HasCallStack =>
  String ->
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnn_ msg = void . submitTxAnn msg

getRewardAccountFor ::
  Credential 'Staking (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (RewardAccount (EraCrypto (BabelEra StandardCrypto)))
getRewardAccountFor stakingC = do
  networkId <- use (to impGlobals . to networkId)
  pure $ RewardAccount networkId stakingC

registerRewardAccount ::
  HasCallStack =>
  ImpTestM (BabelEra StandardCrypto) (RewardAccount (EraCrypto (BabelEra StandardCrypto)))
registerRewardAccount = do
  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  (_, kpSpending) <- freshKeyPair
  let stakingCredential = KeyHashObj khDelegator
  submitTxAnn_ ("Register Reward Account: " <> T.unpack (credToText stakingCredential)) $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . outputsTxBodyL
      .~ SSeq.fromList
        [ mkBasicTxOut
            (mkAddr (kpSpending, kpDelegator))
            (inject $ Coin 10_000_000)
        ]
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.fromList [RegTxCert @(BabelEra StandardCrypto) stakingCredential]
  networkId <- use (to impGlobals . to networkId)
  pure $ RewardAccount networkId stakingCredential

lookupReward ::
  HasCallStack =>
  Credential 'Staking (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) Coin
lookupReward stakingCredential = do
  umap <- getsNES (nesEsL . epochStateUMapL)
  case UMap.lookup stakingCredential (RewDepUView umap) of
    Nothing ->
      error $
        "Staking Credential is not found in the state: "
          <> show stakingCredential
          <> "\nMake sure you have the reward account registered with `registerRewardAccount` "
          <> "or by some other means."
    Just rd -> pure $ fromCompact (rdReward rd)

registerPool ::
  ImpTestM (BabelEra StandardCrypto) (KeyHash 'StakePool (EraCrypto (BabelEra StandardCrypto)))
registerPool = do
  khPool <- freshKeyHash
  rewardAccount <- registerRewardAccount
  vrfHash <- freshKeyHashVRF
  let
    poolParams =
      PoolParams
        { ppVrf = vrfHash
        , ppRewardAccount = rewardAccount
        , ppRelays = mempty
        , ppPledge = zero
        , ppOwners = mempty
        , ppMetadata = SNothing
        , ppMargin = def
        , ppId = khPool
        , ppCost = zero
        }
  submitTxAnn_ "Registering a new stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton (RegPoolTxCert poolParams)
  pure khPool

registerAndRetirePoolToMakeReward ::
  Credential 'Staking (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) ()
registerAndRetirePoolToMakeReward stakingC = do
  poolKH <- freshKeyHash
  networkId <- use (to impGlobals . to networkId)
  vrfKH <- freshKeyHashVRF
  Positive pledge <- arbitrary
  Positive cost <- arbitrary
  let poolParams =
        PoolParams
          { ppVrf = vrfKH
          , ppId = poolKH
          , ppRewardAccount = RewardAccount networkId stakingC
          , ppPledge = Coin pledge
          , ppCost = Coin cost
          , ppOwners = mempty
          , ppMetadata = SNothing
          , ppMargin = def
          , ppRelays = mempty
          }
  submitTxAnn_ "Registering a temporary stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton (RegPoolTxCert poolParams)
  passEpoch
  currentEpochNo <- getsNES nesELL
  submitTxAnn_ "Retiring the temporary stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton (RetirePoolTxCert poolKH $ addEpochInterval currentEpochNo $ EpochInterval 2)
  passEpoch

-- | Compose given function with the configured fixup
withCustomFixup ::
  ( (Tx (BabelEra StandardCrypto) -> ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))) ->
    Tx (BabelEra StandardCrypto) ->
    ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
  ) ->
  ImpTestM (BabelEra StandardCrypto) a ->
  ImpTestM (BabelEra StandardCrypto) a
withCustomFixup f = local $ iteFixupL %~ f

-- | Replace all fixup with the given function
withFixup ::
  (Tx (BabelEra StandardCrypto) -> ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) a ->
  ImpTestM (BabelEra StandardCrypto) a
withFixup f = withCustomFixup (const f)

-- | Performs the action without running the fix-up function on any transactions
withNoFixup :: ImpTestM (BabelEra StandardCrypto) a -> ImpTestM (BabelEra StandardCrypto) a
withNoFixup = withFixup pure

-- | Apply given fixup function before the configured fixup
withPreFixup ::
  (Tx (BabelEra StandardCrypto) -> ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) a ->
  ImpTestM (BabelEra StandardCrypto) a
withPreFixup f = withCustomFixup (f >=>)

-- | Apply given fixup function after the configured fixup
withPostFixup ::
  (Tx (BabelEra StandardCrypto) -> ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) a ->
  ImpTestM (BabelEra StandardCrypto) a
withPostFixup f = withCustomFixup (>=> f)

expectRegisteredRewardAddress ::
  RewardAccount (EraCrypto (BabelEra StandardCrypto)) -> ImpTestM (BabelEra StandardCrypto) ()
expectRegisteredRewardAddress (RewardAccount _ cred) = do
  umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
  Map.member cred (rdPairMap umap) `shouldBe` True

expectNotRegisteredRewardAddress ::
  RewardAccount (EraCrypto (BabelEra StandardCrypto)) -> ImpTestM (BabelEra StandardCrypto) ()
expectNotRegisteredRewardAddress (RewardAccount _ cred) = do
  umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
  Map.member cred (rdPairMap umap) `shouldBe` False

expectTreasury :: HasCallStack => Coin -> ImpTestM (BabelEra StandardCrypto) ()
expectTreasury c =
  impAnn "Checking treasury amount" $ do
    treasuryAmt <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    c `shouldBe` treasuryAmt

impGetNativeScript ::
  ScriptHash (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (Maybe (NativeScript (BabelEra StandardCrypto)))
impGetNativeScript sh = Map.lookup sh <$> gets impNativeScripts

impLookupUTxO ::
  TxIn (EraCrypto (BabelEra StandardCrypto)) ->
  ImpTestM (BabelEra StandardCrypto) (TxOut (BabelEra StandardCrypto))
impLookupUTxO txIn = impAnn "Looking up TxOut" $ do
  utxo <- getUTxO
  case txinLookup txIn utxo of
    Just txOut -> pure txOut
    Nothing -> error $ "Failed to get TxOut for " <> show txIn
