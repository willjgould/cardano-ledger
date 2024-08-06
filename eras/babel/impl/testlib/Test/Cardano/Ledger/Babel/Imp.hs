{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Cardano.Ledger.Babel.Imp (spec) where

import Cardano.Chain.UTxO (balance)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Ledger.Address (BootstrapAddress)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (ValidationTagMismatch),
  TagMismatchDescription (PassedUnexpectedly),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx, IsValid (IsValid), bodyAlonzoTxL)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Governance ()
import Cardano.Ledger.Babel.LedgerState.Types
import Cardano.Ledger.Babel.Rules (
  BabelLedgersEnv (BabelLedgersEnv),
  BabelUtxoPredFailure (..),
 )
import Cardano.Ledger.Babel.TxInfo (BabelContextError)
import Cardano.Ledger.BaseTypes (
  Globals,
  Inject (inject),
  Network (..),
  SlotNo,
  TxIx (TxIx),
  mkTxIxPartial,
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayNewEpochEvent,
  PredicateFailure,
 )
import Cardano.Ledger.Credential (StakeReference (StakeRefNull))
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.Keys
import Cardano.Ledger.Plutus (hashPlutusScript)
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..))
import Cardano.Ledger.Shelley.API (
  Addr (..),
  Credential (..),
  LedgerEnv (..),
  NewEpochState,
  TxId (..),
  TxIn (TxIn),
  UTxO (UTxO),
 )
import Cardano.Ledger.Shelley.LedgerState (
  HasLedgerState (from),
  LedgerState,
  curPParamsEpochStateL,
  esAccountStateL,
  esLStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (Event, ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import Cardano.Ledger.TxIn (Fulfill)
import Cardano.Ledger.Val (coin)
import qualified Cardano.Ledger.Val as Val
import Control.Monad.RWS (MonadIO (liftIO), MonadState (..), MonadWriter (..), gets)
import Control.Monad.RWS.Class (asks, modify)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (toList), traverse_)
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Debug.Trace (trace)
import qualified GHC.Base as Base
import Lens.Micro (folded, (&), (.~), (<>~), (^.), (^..))
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl ((.=))
import Lens.Micro.Type (Getting, Lens')
import Prettyprinter (Doc)
import Test.Cardano.Ledger.Alonzo.ImpTest (SomeSTSEvent (..))
import qualified Test.Cardano.Ledger.Babbage.Imp as BabbageImp
import qualified Test.Cardano.Ledger.Babel.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Babel.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Babel.Imp.GovCertSpec as GovCert
import qualified Test.Cardano.Ledger.Babel.Imp.GovSpec as Gov
import qualified Test.Cardano.Ledger.Babel.Imp.RatifySpec as Ratify
import qualified Test.Cardano.Ledger.Babel.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babel.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Babel.ImpTest (
  ImpTestEnv (..),
  ImpTestM,
  ImpTestState (..),
  freshKeyAddr,
  getUTxO,
  impAnn,
  impLSTL,
  impLedgerEnv,
  impNESL,
  impRootTxInL,
  logToExpr,
  lookupImpRootTxOut,
  makeCollateralInput,
  tryRunImpRule,
  updateAddrTxWits,
  withImpStateWithProtVer,
 )
import Test.Cardano.Ledger.Common hiding (shouldBeLeftExpr)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraExpectation)
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common (shouldBeLeftExpr)
import qualified Test.Cardano.Ledger.Imp.Common as Imp
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)
import Test.QuickCheck.Random (QCGen)

spec ::
  forall era.
  ( era ~ BabelEra StandardCrypto
  , GovState era ~ ConwayGovState era
  , PParamsHKD Identity era ~ ConwayPParams Identity era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (BabelContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , NFData (Event (EraRule "ENACT" era))
  , ToExpr (Event (EraRule "ENACT" era))
  , Eq (Event (EraRule "ENACT" era))
  , Typeable (Event (EraRule "ENACT" era))
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  Spec
spec =
  describe "BabelImpSpec - post bootstrap (protocol version 10)" $
    withImpStateWithProtVer @StandardCrypto (natVersion @10) $ do
      -- spec2
      zoneSpec
      utxoSpec

-- Enact.spec @era
-- Epoch.spec @era
-- Gov.spec @era
-- GovCert.spec @era
-- Utxo.spec @era
-- Utxos.spec @era
-- Ratify.spec @era
-- describe "BabelImpSpec - bootstrap phase (protocol version 9)" $
--   withImpState @LedgerStateTemp @era $ do
--     Enact.relevantDuringBootstrapSpec @era
--     Epoch.relevantDuringBootstrapSpec @era
--     Gov.relevantDuringBootstrapSpec @era
--     GovCert.relevantDuringBootstrapSpec @era
--     Utxo.spec @era
--     Utxos.relevantDuringBootstrapSpec @era
--     Ratify.relevantDuringBootstrapSpec @era

submitFulfillTx ::
  ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
submitFulfillTx = do
  st <- gets impNES
  lEnv <- impLedgerEnv st
  ImpTestState {impRootTxIn, impLST} <- get
  (_, addr) <- freshKeyAddr
  fmap (txInAt @Int @(BabelEra StandardCrypto) (0 :: Int))
    . submitTxAnn "Sumbit a transaction with a script output"
    $ mkBasicTx mkBasicTxBody
      & bodyTxL
      . outputsTxBodyL
      .~ SSeq.singleton
        ( mkBasicTxOut
            addr -- (Addr Testnet (ScriptHashObj $ hashPlutusScript (guessTheNumber3 SPlutusV4)) StakeRefNull)
            (inject (Coin 100))
        )

submitTxAnn_ ::
  HasCallStack =>
  String ->
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnn_ msg = void . submitTxAnn msg

submitTxAnn ::
  HasCallStack =>
  String ->
  Tx (BabelEra StandardCrypto) ->
  ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
submitTxAnn msg tx = impAnn msg (trySubmitTx tx >>= Imp.expectRightDeepExpr)

trySubmitTx ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (Base.NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        (Tx (BabelEra StandardCrypto))
    )
trySubmitTx tx = do
  txFixed <- asks iteFixup >>= ($ tx)
  logToExpr txFixed
  st <- gets impNES
  let newEpochStateLedgerState = from $ st ^. nesEsL . esLStateL
  lEnv <- impLedgerEnv st
  impLSTL
    . lstUtxoStateL
    . utxostGovStateL
    .= (newEpochStateLedgerState ^. lstUtxoStateL . utxostGovStateL)
  ImpTestState {impRootTxIn, impLST} <- get

  res <-
    tryRunImpRule
      @"LEDGER"
      lEnv
      impLST
      txFixed

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
      tell $
        fmap (SomeSTSEvent @(BabelEra StandardCrypto) @"LEDGER") events
      modify $ impLSTL .~ st'
      modify $ impNESL . nesEsL . esLStateL .~ from st'
      stgWord32ToFloat <- gets impNES
      st2 <- gets impNES
      ImpTestState {impLST} <- get
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

trySubmitZone ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (Base.NonEmpty (PredicateFailure (EraRule "ZONE" (BabelEra StandardCrypto))))
        (Tx (BabelEra StandardCrypto))
    )
trySubmitZone tx = do
  txFixed <- asks iteFixup >>= ($ tx)
  logToExpr txFixed
  st <- gets impNES
  lEnv <- impLedgerEnv st
  ImpTestState {impRootTxIn} <- get
  LedgerEnv sn _ pp accSt <- impLedgerEnv st
  res <-
    tryRunImpRule @"ZONE"
      (BabelLedgersEnv sn (TxIx 0) pp accSt)
      (from $ st ^. nesEsL . esLStateL)
      (Seq.singleton txFixed)
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
      tell $ fmap (SomeSTSEvent @(BabelEra StandardCrypto) @"ZONE") events
      modify $ impNESL . nesEsL . esLStateL .~ from st'
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

submitTxAnnZone_ ::
  HasCallStack =>
  Word64 ->
  String ->
  [Tx (BabelEra StandardCrypto)] ->
  [Maybe (Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto))] ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnnZone_ ixStart msg txs fs = void $ submitTxAnnZone ixStart msg txs fs mempty

submitTxAnnZone ::
  HasCallStack =>
  Word64 ->
  String ->
  [Tx (BabelEra StandardCrypto)] ->
  [Maybe (Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto))] ->
  [PredicateFailure (EraRule "ZONE" (BabelEra StandardCrypto))] ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnnZone ixStart msg tx fulfills expectedErrors = do
  res ::
    Either
      (Base.NonEmpty (PredicateFailure (EraRule "ZONE" (BabelEra StandardCrypto))))
      [Tx (BabelEra StandardCrypto)] <-
    zone ixStart tx fulfills
  case expectedErrors of
    [] -> void $ impAnn msg (Imp.expectRightDeepExpr res)
    (e : es) -> impAnn msg (shouldBeLeftExpr res (e Base.:| es))

submitFailingZone ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  Maybe (Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)) ->
  Base.NonEmpty (PredicateFailure (EraRule "ZONE" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingZone tx fulfills expectedFailure = trySubmitZone tx >>= (`shouldBeLeftExpr` expectedFailure)

zone ::
  HasCallStack =>
  Word64 ->
  [Tx (BabelEra StandardCrypto)] ->
  [Maybe (Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto))] ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (Base.NonEmpty (PredicateFailure (EraRule "ZONE" (BabelEra StandardCrypto))))
        [Tx (BabelEra StandardCrypto)]
    )
zone ixStart tx fulfills = do
  -- If you're actually sticking with this collateral hack (just for the prototype), at least make it `zipWith` the txs
  -- collateralInputTxs <- makeCollateralInput
  txFixed' :: [AlonzoTx (BabelEra StandardCrypto)] <- asks iteFixup >>= for tx
  -- I'm removing the outputs that the fixup adds
  txFixed :: [AlonzoTx (BabelEra StandardCrypto)] <-
    traverse
      ( updateAddrTxWits
          . ( \tx ->
                if not $ null $ tx ^. bodyTxL . fulfillsTxBodyL
                  then
                    tx
                      & bodyTxL
                      . outputsTxBodyL
                      .~ mempty
                      & witsTxL
                      . addrTxWitsL
                      .~ mempty
                  else -- & bodyTxL
                  -- . collateralInputsTxBodyL
                  -- .~ Set.singleton collateralInputTxs
                    tx
            )
      )
      txFixed'
  let fulfillTxs =
        catMaybes $
          zipWith
            (\tx f -> fmap (\f' -> f' (Set.singleton (txInAt (0 :: Int) tx))) f)
            txFixed
            fulfills

  -- collateralInputFfs <- makeCollateralInput

  fixedFulfills' <- asks iteFixup >>= for fulfillTxs
  -- I'm removing the outputs and inputs that the fixup adds
  fixedFulfills <-
    traverse
      ( updateAddrTxWits
          . ( \tx ->
                if not $ null $ tx ^. bodyTxL . fulfillsTxBodyL
                  then
                    tx
                      & bodyTxL
                      . outputsTxBodyL
                      .~ mempty
                      & bodyTxL
                      . inputsTxBodyL
                      -- This ridiculous hack is to remove a problematic input being added by the fixups that happens to be at ix 4 in this case
                      .~ Set.filter (\(TxIn _ (TxIx ix)) -> ix /= 4) (tx ^. bodyTxL . inputsTxBodyL)
                      & witsTxL
                      . addrTxWitsL
                      .~ mempty
                  else -- & bodyTxL
                  -- . collateralInputsTxBodyL
                  -- .~ Set.singleton collateralInputFfs
                    tx
            )
      )
      fixedFulfills'

  traverse_
    logToExpr
    txFixed
  st <- gets impNES
  let newEpochStateLedgerState = st ^. nesEsL . esLStateL
  LedgerEnv sn _ pp accSt <- impLedgerEnv st

  ImpTestState {impRootTxIn, impLST} <- get

  res <-
    tryRunImpRule
      @"ZONE"
      (BabelLedgersEnv sn (TxIx ixStart) pp accSt)
      newEpochStateLedgerState
      (Seq.fromList (txFixed <> fixedFulfills))

  case res of
    Left predFailures -> do
      -- Verify that produced predicate failures are ready for the node-to-client protocol
      liftIO $ forM_ predFailures $ roundTripEraExpectation @(BabelEra StandardCrypto)
      pure $ Left predFailures
    Right (st' :: LedgerState (BabelEra StandardCrypto), events) -> do
      let
        txId = TxId . hashAnnotated $ last txFixed ^. bodyTxL
        outsSize = length $ fmap toList $ txFixed ^.. folded . bodyTxL . outputsTxBodyL
        rootIndex
          | outsSize > 0 = outsSize - 1
          | otherwise = error ("Expected at least 1 output after submitting tx: " <> show "fix me")
      tell $
        fmap (SomeSTSEvent @(BabelEra StandardCrypto) @"ZONE") events
      modify $ impLSTL .~ def
      modify $ impNESL . nesEsL . esLStateL .~ from st'
      stgWord32ToFloat <- gets impNES
      st2 <- gets impNES
      ImpTestState {impLST} <- get
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

zoneSpec ::
  SpecWith (ImpTestState (BabelEra StandardCrypto))
zoneSpec = describe "ZONE" $ do
  it "fails with requiredTx from different zone" $ do
    (_, addr1) <- freshKeyAddr
    (_, addr2) <- freshKeyAddr
    (_, addr3) <- freshKeyAddr

    tx <-
      submitTxAnn
        "Sumbit a transaction with a script output"
        $ mkBasicTx mkBasicTxBody
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.fromList
            [ ffTx addr1 972746368
            , ffTx addr2 26904150
            , ffTx addr3 179361
            ]
    let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
        tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
        tx3 = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
    void $
      submitFailingZone
        ( mkBasicTx mkBasicTxBody
            & bodyTxL
            . requiredTxsTxBodyL
            .~ Set.singleton tx1
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton tx2
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx3
        )
        Nothing
        (injectFailure (CheckRqTxFailure @(BabelEra StandardCrypto)) Base.:| [])
  it "succeeds with a simple zone submission" $ do
    (requestTx, fulfillTx) <- makeTestTransactions

    submitTxAnnZone_
      0
      "Submit a transaction that consumes the script output"
      [requestTx]
      [Just fulfillTx]
  it "IsValid erroneously set to False: `PassedUnexpectedly`" $ do
    (requestTx, fulfillTx) <- makeTestTransactions

    let fulfillTx' ::
          Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
        fulfillTx' =
          ( isValidTxL
              .~ IsValid False
          )
            . fulfillTx
    void $
      submitTxAnnZone
        0
        "Submit a transaction that consumes the script output"
        [requestTx]
        [Just fulfillTx']
        [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]
  it "IsValid set to False in preceding tx" $ do
    -- This test, in conjunction with the one above, proves that only the last transaction can be invalid
    (requestTx, fulfillTx) <- makeTestTransactions

    let fulfillTx' ::
          Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
        fulfillTx' =
          fulfillTx
    void $
      submitTxAnnZone
        0
        "Submit a transaction that consumes the script output"
        [ requestTx
            & isValidTxL
            .~ IsValid False
        ]
        [Just fulfillTx']
        [injectFailure MoreThanOneInvalidTransaction]
  it "ZONE-V: Collateral isn't sufficient to cover preceding transactions" $
    do
      st <- gets impNES
      lEnv <- impLedgerEnv st
      (_, addr1) <- freshKeyAddr
      (_, addr2) <- freshKeyAddr
      (_, addr3) <- freshKeyAddr
      (_, addr4) <- freshKeyAddr
      (_, addrCollatReq) <- freshKeyAddr
      (_, addrCollatFf) <- freshKeyAddr

      rootTxIn <- fst <$> lookupImpRootTxOut

      tx <-
        submitTxAnn
          "Sumbit a transaction with a script output"
          $ mkBasicTx mkBasicTxBody
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.fromList
              [ ffTx addr1 445802143
              , ffTx addr2 499911058
              , ffTx addrCollatReq 27108750
              , ffTx addrCollatFf 27006300
              ]

      let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
          tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
          txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
          txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

      let
        requestTx' =
          mkBasicTx mkBasicTxBody
            & bodyTxL
            . requestsTxBodyL
            .~ SSeq.singleton (ffTx addr3 499731741)
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx1
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.singleton (ffTx addr4 945353159)
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton txCollatReq

      let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
          fulfillTx fulfills =
            mkBasicTx mkBasicTxBody
              & bodyTxL
              . fulfillsTxBodyL
              .~ fulfills
              & bodyTxL
              . inputsTxBodyL
              .~ Set.singleton tx2
              & bodyTxL
              . collateralInputsTxBodyL
              .~ Set.singleton txCollatFf

      void $
        submitTxAnnZone
          0
          "Submit a transaction that consumes the script output"
          [requestTx']
          [Just fulfillTx]
          [injectFailure $ CollForPrecValidFailure (Coin 27000000)]
  it "ZONE-N: Collateral isn't sufficient to cover preceding transactions" $
    do
      st <- gets impNES
      lEnv <- impLedgerEnv st
      (_, addr1) <- freshKeyAddr
      (_, addr2) <- freshKeyAddr
      (_, addr3) <- freshKeyAddr
      (_, addr4) <- freshKeyAddr
      (_, addrCollatReq) <- freshKeyAddr
      (_, addrCollatFf) <- freshKeyAddr

      rootTxIn <- fst <$> lookupImpRootTxOut

      tx <-
        submitTxAnn
          "Sumbit a transaction with a script output"
          $ mkBasicTx mkBasicTxBody
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.fromList
              [ ffTx addr1 445802143
              , ffTx addr2 499911058
              , ffTx addrCollatReq 27108750
              , ffTx addrCollatFf 27006300
              ]

      let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
          tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
          txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
          txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

      let
        requestTx' =
          mkBasicTx mkBasicTxBody
            & bodyTxL
            . requestsTxBodyL
            .~ SSeq.singleton (ffTx addr3 499731741)
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx1
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.singleton (ffTx addr4 945353159)
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton txCollatReq

      let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
          fulfillTx fulfills =
            mkBasicTx mkBasicTxBody
              & bodyTxL
              . fulfillsTxBodyL
              .~ fulfills
              & bodyTxL
              . inputsTxBodyL
              .~ Set.singleton tx2
              & bodyTxL
              . collateralInputsTxBodyL
              .~ Set.singleton txCollatFf
              & isValidTxL
              .~ IsValid False

      void $
        submitTxAnnZone
          0
          "Submit a transaction that consumes the script output"
          [requestTx']
          [Just fulfillTx]
          [injectFailure $ CollForPrecValidFailure (Coin 27000000)]

ffTx :: EraTxOut era => Addr (EraCrypto era) -> Integer -> TxOut era
ffTx addr' amt =
  mkBasicTxOut
    addr'
    (inject (Coin amt))

utxoSpec ::
  SpecWith (ImpTestState (BabelEra StandardCrypto))
utxoSpec = describe "UTXO" $ do
  it "fails with a fulfills not in FRxO error" $ do
    st <- gets impNES
    lEnv <- impLedgerEnv st
    (_, addr1) <- freshKeyAddr
    (_, addr2) <- freshKeyAddr
    (_, addr3) <- freshKeyAddr
    (_, addr4) <- freshKeyAddr
    (_, addrCollatReq) <- freshKeyAddr
    (_, addrCollatFf) <- freshKeyAddr

    rootTxIn <- fst <$> lookupImpRootTxOut

    tx <-
      submitTxAnn
        "Sumbit a transaction with a script output"
        $ mkBasicTx mkBasicTxBody
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.fromList
            [ ffTx addr1 418562959
            , ffTx addr2 499912642
            , ffTx addrCollatReq 27108750
            , ffTx addrCollatFf 54243900
            ]

    let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
        tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
        txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
        txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

    let
      requestTx' =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . requestsTxBodyL
          .~ SSeq.singleton (ffTx addr3 499731741)
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton tx1
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.singleton (ffTx addr4 918113975)
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatReq

    let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
        fulfillTx fulfills =
          mkBasicTx mkBasicTxBody
            & bodyTxL
            . fulfillsTxBodyL
            .~ fulfills
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx2
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton txCollatFf

    let badFulfill = Set.singleton (txInAt (10 :: Int) requestTx')

    submitTxAnnZone
      0
      "Submit a transaction that consumes the script output"
      [requestTx']
      [Just ((bodyTxL . fulfillsTxBodyL <>~ badFulfill) . fulfillTx)]
      [injectFailure $ BadFulfillsFRxO badFulfill]
  it "fails with ValueNotConservedUTxO due to outputs" $ do
    (_, addr1) <- freshKeyAddr
    (requestTx, fulfillTx) <- makeTestTransactions

    submitTxAnnZone
      0
      "Submit a transaction that consumes the script output"
      [requestTx & bodyTxL . outputsTxBodyL <>~ SSeq.singleton (ffTx addr1 1)]
      [Just fulfillTx]
      [ injectFailure $ ValueNotConservedUTxO (Val.inject (Coin 918533884)) (Val.inject (Coin 918535337))
      , injectFailure $ CollForPrecValidFailure (Coin 217800)
      ]
  it "fails with ValueNotConservedUTxO due to requests" $ do
    (_, addr1) <- freshKeyAddr
    (requestTx, fulfillTx) <- makeTestTransactions

    submitTxAnnZone
      0
      "Submit a transaction that consumes the script output"
      [requestTx & bodyTxL . requestsTxBodyL <>~ SSeq.singleton (ffTx addr1 1)]
      [Just fulfillTx]
      [ injectFailure $ ValueNotConservedUTxO (Val.inject (Coin 918533885)) (Val.inject (Coin 918535336))
      , injectFailure $ CollForPrecValidFailure (Coin 217800)
      ]

-- TODO WG add witness test if possible once we're sure the witness logic is correct

makeTestTransactions ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( Tx (BabelEra StandardCrypto)
    , Set (Fulfill StandardCrypto) -> AlonzoTx (BabelEra StandardCrypto)
    )
makeTestTransactions = do
  st <- gets impNES
  lEnv <- impLedgerEnv st
  (_, addr1) <- freshKeyAddr
  (_, addr2) <- freshKeyAddr
  (_, addr3) <- freshKeyAddr
  (_, addr4) <- freshKeyAddr
  (_, addrCollatReq) <- freshKeyAddr
  (_, addrCollatFf) <- freshKeyAddr

  rootTxIn <- fst <$> lookupImpRootTxOut

  tx <-
    submitTxAnn
      "Sumbit a transaction with a script output"
      $ mkBasicTx mkBasicTxBody
        & bodyTxL
        . outputsTxBodyL
        .~ SSeq.fromList
          [ ffTx addr1 418802143
          , ffTx addr2 499911058
          , ffTx addrCollatReq 27108750
          , ffTx addrCollatFf 54006300
          ]

  let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
      tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
      txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
      txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

  let
    requestTx' =
      mkBasicTx mkBasicTxBody
        & bodyTxL
        . requestsTxBodyL
        .~ SSeq.singleton (ffTx addr3 499731741)
        & bodyTxL
        . inputsTxBodyL
        .~ Set.singleton tx1
        & bodyTxL
        . outputsTxBodyL
        .~ SSeq.singleton (ffTx addr4 918353159)
        & bodyTxL
        . collateralInputsTxBodyL
        .~ Set.singleton txCollatReq

  let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
      fulfillTx fulfills =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . fulfillsTxBodyL
          .~ fulfills
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton tx2
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatFf

  pure (requestTx', fulfillTx)