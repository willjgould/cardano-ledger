{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Cardano.Ledger.Babel.Imp (spec) where

import Cardano.Chain.UTxO (balance)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Ledger.Address (BootstrapAddress)
import Cardano.Ledger.Allegra.Scripts (Timelock (RequireAllOf))
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
import Cardano.Ledger.Babel.Rules (BabelUtxoPredFailure (..), BabelUtxowPredFailure (..))
import Cardano.Ledger.Babel.Tx
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody (..))
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
import Cardano.Ledger.Keys hiding (KeyPair)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
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
  LedgerState,
  curPParamsEpochStateL,
  esAccountStateL,
  esLStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (Event, ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.TxIn (Fulfill)
import Cardano.Ledger.Val (Val (..), coin)
import qualified Cardano.Ledger.Val as Val
import Control.Monad.RWS (MonadIO (liftIO), MonadState (..), MonadWriter (..), gets)
import Control.Monad.RWS.Class (asks, modify)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (toList), traverse_)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict (StrictMaybe (..))
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
  impLedgerEnv,
  impNESL,
  impRootTxInL,
  largeValue,
  logToExpr,
  lookupImpRootTxOut,
  makeCollateralInput,
  mediumValue,
  smallValue,
  submitTxAnn,
  totalSupply,
  tryRunImpRule,
  updateAddrTxWits,
  withImpStateWithProtVer,
 )
import Test.Cardano.Ledger.Common hiding (shouldBeLeftExpr)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraExpectation)
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair, KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common (shouldBeLeftExpr)
import qualified Test.Cardano.Ledger.Imp.Common as Imp
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.QuickCheck.Random (QCGen)

spec ::
  Spec
spec =
  describe "BabelImpSpec - post bootstrap (protocol version 10)" $
    withImpStateWithProtVer (natVersion @10) $
      do
        -- spec2
        zoneSpec

-- -- utxoSpec

-- -- Enact.spec @era
-- -- Epoch.spec @era
-- -- Gov.spec @era
-- -- GovCert.spec @era
-- -- Utxo.spec @era
-- -- Utxos.spec @era
-- -- Ratify.spec @era
-- -- describe "BabelImpSpec - bootstrap phase (protocol version 9)" $
-- --   withImpState @era $ do
-- --     Enact.relevantDuringBootstrapSpec @era
-- --     Epoch.relevantDuringBootstrapSpec @era
-- --     Gov.relevantDuringBootstrapSpec @era
-- --     GovCert.relevantDuringBootstrapSpec @era
-- --     Utxo.spec @era
-- --     Utxos.relevantDuringBootstrapSpec @era
-- --     Ratify.relevantDuringBootstrapSpec @era

trySubmitZone ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (Base.NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
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
    tryRunImpRule @"LEDGER"
      (LedgerEnv sn (TxIx 0) pp accSt)
      (st ^. nesEsL . esLStateL)
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

submitTxAnnZone_ ::
  HasCallStack =>
  Int ->
  Word64 ->
  String ->
  [Tx (BabelEra StandardCrypto)] ->
  [Tx (BabelEra StandardCrypto)] ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnnZone_ n ixStart msg txs fs = void $ submitTxAnnZone n ixStart msg txs fs mempty

submitTxAnnZone ::
  HasCallStack =>
  Int ->
  Word64 ->
  String ->
  [Tx (BabelEra StandardCrypto)] ->
  [Tx (BabelEra StandardCrypto)] ->
  [PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))] ->
  ImpTestM (BabelEra StandardCrypto) ()
submitTxAnnZone n ixStart msg tx fulfills expectedErrors = do
  res ::
    Either
      (Base.NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
      [Tx (BabelEra StandardCrypto)] <-
    zone n ixStart tx fulfills
  case expectedErrors of
    [] -> void $ impAnn msg (Imp.expectRightDeepExpr res)
    (e : es) -> impAnn msg (shouldBeLeftExpr res (e Base.:| es))

submitFailingZone ::
  HasCallStack =>
  Tx (BabelEra StandardCrypto) ->
  Maybe (Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)) ->
  Base.NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))) ->
  ImpTestM (BabelEra StandardCrypto) ()
submitFailingZone tx fulfills expectedFailure = trySubmitZone tx >>= (`shouldBeLeftExpr` expectedFailure)

zone ::
  HasCallStack =>
  Int ->
  Word64 ->
  [Tx (BabelEra StandardCrypto)] ->
  [Tx (BabelEra StandardCrypto)] ->
  ImpTestM
    (BabelEra StandardCrypto)
    ( Either
        (Base.NonEmpty (PredicateFailure (EraRule "LEDGER" (BabelEra StandardCrypto))))
        [Tx (BabelEra StandardCrypto)]
    )
zone n ixStart tx fulfills = do
  -- If you're actually sticking with this collateral hack (just for the prototype), at least make it `zipWith` the txs
  -- collateralInputTxs <- makeCollateralInput
  txFixed' :: [BabelTx (BabelEra StandardCrypto)] <- asks iteFixup >>= for tx
  -- I'm removing the outputs that the fixup adds
  txFixed :: [BabelTx (BabelEra StandardCrypto)] <-
    traverse
      ( updateAddrTxWits
          . ( \tx ->
                tx
                  & witsTxL
                  . addrTxWitsL
                  .~ mempty
                  & bodyTxL
                  . inputsTxBodyL
                  -- This ridiculous hack is to remove a problematic input being added by the fixups that happens to be at ix 4 in this case
                  .~ Set.filter
                    (\(TxIn _ (TxIx ix)) -> ix /= fromIntegral n)
                    (tx ^. bodyTxL . inputsTxBodyL)
            )
      )
      txFixed'

  -- collateralInputFfs <- makeCollateralInput

  fixedFulfills' <- asks iteFixup >>= for fulfills
  -- I'm removing the outputs and inputs that the fixup adds
  fixedFulfills <-
    traverse
      ( updateAddrTxWits
          . ( \tx ->
                tx
                  & bodyTxL
                  . inputsTxBodyL
                  -- This ridiculous hack is to remove a problematic input being added by the fixups that happens to be at ix 4 in this case
                  .~ Set.filter
                    (\(TxIn _ (TxIx ix)) -> ix /= fromIntegral n)
                    (tx ^. bodyTxL . inputsTxBodyL)
                  & witsTxL
                  . addrTxWitsL
                  .~ mempty
                  & bodyTxL
                  . outputsTxBodyL
                  .~ SSeq.singleton
                    ( head $
                        Foldable.toList $
                          tx
                            ^. bodyTxL
                            . outputsTxBodyL
                    )
            )
      )
      fixedFulfills'

  traverse_
    logToExpr
    txFixed
  st <- gets impNES
  let newEpochStateLedgerState = st ^. nesEsL . esLStateL
  LedgerEnv sn _ pp accSt <- impLedgerEnv st

  ImpTestState {impRootTxIn} <- get

  let parentTx :: BabelTx (BabelEra StandardCrypto)
      parentTx =
        mkBasicBabelTx mkBasicTxBody
          & subTxTxL
          .~ SJust (SSeq.fromList $ txFixed ++ fixedFulfills)
          & bodyTxL
          . swapsTxBodyL
          .~ Set.fromList (fmap txIdTx txFixed ++ fmap txIdTx fixedFulfills)

  res <-
    tryRunImpRule
      @"LEDGER"
      (LedgerEnv sn (TxIx ixStart) pp accSt)
      newEpochStateLedgerState
      parentTx

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
        fmap (SomeSTSEvent @(BabelEra StandardCrypto) @"LEDGER") events
      modify $ impNESL . nesEsL . esLStateL .~ st'
      stgWord32ToFloat <- gets impNES
      st2 <- gets impNES
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
zoneSpec = describe "SWAPS" $ do
  it
    "succeeds with a simple zone submission"
    $ do
      (requestTx, fulfillTx) <- makeTestTransactions

      submitTxAnnZone_
        7
        0
        "Submit a transaction that consumes the script output"
        [requestTx]
        [fulfillTx]

  it "missing is supplied by two transactions" $ do
    st <- gets impNES
    lEnv <- impLedgerEnv st
    (_, addr1) <- freshKeyAddr
    (_, addr2) <- freshKeyAddr
    (_, addr3) <- freshKeyAddr
    (_, addr4) <- freshKeyAddr
    (_, addrMa1) <- freshKeyAddr
    (_, addrMa2) <- freshKeyAddr
    (_, addrRest) <- freshKeyAddr
    (_, addrCollatReq1) <- freshKeyAddr
    (_, addrCollatReq2) <- freshKeyAddr
    (_, addrCollatFf) <- freshKeyAddr

    rootTxIn <- fst <$> lookupImpRootTxOut

    tx <-
      submitTxAnn
        "Sumbit a transaction with a script output"
        $ mkBasicTx mkBasicTxBody
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.fromList
            [ ffTx addr1 438935756 mempty
            , ffTx addr2 479671124 mempty
            , ffTx addrCollatReq1 27108750 mempty
            , ffTx addrCollatReq2 54012900 mempty
            , ffTx addrCollatFf 81326250 mempty
            , ffTx addrMa1 0 smallValue
            , ffTx addrMa2 0 smallValue
            , ffTx addrRest 0 largeValue
            ]

    let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
        tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
        txCollatReq1 = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
        txCollatReq2 = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx
        txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (4 :: Int) tx
        txMa1 = txInAt @Int @(BabelEra StandardCrypto) (5 :: Int) tx
        txMa2 = txInAt @Int @(BabelEra StandardCrypto) (6 :: Int) tx

    let
      requestTx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton txMa1
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.singleton (ffTx addr4 219431622 mempty)
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatReq1
      requestTx2 =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton txMa2
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.singleton (ffTx addr4 219431622 mempty)
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatReq2

    let fulfillTx :: Tx (BabelEra StandardCrypto)
        fulfillTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx1
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton txCollatFf
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.singleton (ffTx addr3 0 mediumValue)

    submitTxAnnZone_
      8
      0
      "Submit a transaction that consumes the script output"
      [requestTx1, requestTx2]
      [fulfillTx]

  it "IsValid erroneously set to False: `PassedUnexpectedly`" $ do
    (requestTx, fulfillTx) <- makeTestTransactions

    let fulfillTx' :: Tx (BabelEra StandardCrypto)
        fulfillTx' =
          fulfillTx
            & ( isValidTxL
                  .~ IsValid False
              )
    submitTxAnnZone
      7
      0
      "Submit a transaction that consumes the script output"
      [requestTx]
      [fulfillTx']
      [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]

  it "More than one transaction can be invalid" $ do
    -- it "IsValid set to False in preceding tx" $ do
    -- This test, in conjunction with the one above, proves that only the last transaction can be invalid
    (requestTx, fulfillTx) <- makeTestTransactions
    void $
      submitTxAnnZone
        7
        0
        "Submit a transaction that consumes the script output"
        [ requestTx
            & isValidTxL
            .~ IsValid False
        ]
        [ fulfillTx
            & isValidTxL
            .~ IsValid False
        ]
        [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)] -- [injectFailure MoreThanOneInvalidTransaction]
  it "Subs in subs" $ do
    st <- gets impNES
    lEnv <- impLedgerEnv st
    (_, addr1) <- freshKeyAddr
    (_, addr2) <- freshKeyAddr
    (_, addr3) <- freshKeyAddr
    (_, addr4) <- freshKeyAddr
    (_, addrMa) <- freshKeyAddr
    (_, addrSink1) <- freshKeyAddr
    (_, addrSink2) <- freshKeyAddr
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
            [ ffTx addr1 419213621 mempty
            , ffTx addr2 499548464 mempty
            , ffTx addrCollatReq 27108750 mempty
            , ffTx addrCollatFf 54012900 mempty
            , ffTx addrMa 0 smallValue
            , ffTx addrSink1 0 smallValue
            , ffTx addrSink2 0 largeValue
            ]

    let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
        tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
        txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
        txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx
        txMa = txInAt @Int @(BabelEra StandardCrypto) (4 :: Int) tx

    let
      requestTx' =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton txMa
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.singleton (ffTx addr4 419162229 mempty)
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatReq

    let fulfillTx :: Tx (BabelEra StandardCrypto)
        fulfillTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL
            . inputsTxBodyL
            .~ Set.singleton tx1
            & bodyTxL
            . collateralInputsTxBodyL
            .~ Set.singleton txCollatFf
            & bodyTxL
            . outputsTxBodyL
            .~ SSeq.singleton (ffTx addr3 0 smallValue)

    let illegalSub :: Tx (BabelEra StandardCrypto)
        illegalSub = mkBasicTx mkBasicTxBody

    void $
      submitTxAnnZone
        7
        0
        "Submit a transaction that consumes the script output"
        [ requestTx'
            & subTxTxL
            .~ SJust (SSeq.fromList [illegalSub])
            & bodyTxL
            . swapsTxBodyL
            .~ Set.fromList [txIdTx illegalSub]
        ]
        [fulfillTx]
        [injectFailure SubsInSubsUtxowFailure]

-- it "SWAPS-V: Collateral isn't sufficient to cover preceding transactions" $ do
--   st <- gets impNES
--   lEnv <- impLedgerEnv st
--   (_, addr1) <- freshKeyAddr
--   (_, addr2) <- freshKeyAddr
--   (_, addr3) <- freshKeyAddr
--   (_, addr4) <- freshKeyAddr
--   (_, addrCollatReq) <- freshKeyAddr
--   (_, addrCollatFf) <- freshKeyAddr

--   rootTxIn <- fst <$> lookupImpRootTxOut

--   tx <-
--     submitTxAnn
--       "Sumbit a transaction with a script output"
--       $ mkBasicTx mkBasicTxBody
--         & bodyTxL
--         . outputsTxBodyL
--         .~ SSeq.fromList
--           [ ffTx addr1 445802143 mempty
--           , ffTx addr2 499911058 mempty
--           , ffTx addrCollatReq 27108750 mempty
--           , ffTx addrCollatFf 27006300 mempty
--           ]

--   let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
--       tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
--       txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
--       txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

--   let
--     requestTx' =
--       mkBasicTx mkBasicTxBody
--         & bodyTxL
--         . inputsTxBodyL
--         .~ Set.singleton tx1
--         & bodyTxL
--         . outputsTxBodyL
--         .~ SSeq.singleton (ffTx addr4 945353159 mempty)
--         & bodyTxL
--         . collateralInputsTxBodyL
--         .~ Set.singleton txCollatReq

--   let fulfillTx :: Tx (BabelEra StandardCrypto)
--       fulfillTx =
--         mkBasicTx mkBasicTxBody
--           & bodyTxL
--           . inputsTxBodyL
--           .~ Set.singleton tx2
--           & bodyTxL
--           . collateralInputsTxBodyL
--           .~ Set.singleton txCollatFf

--   void $
--     submitTxAnnZone
--       7
--       0
--       "Submit a transaction that consumes the script output"
--       [requestTx']
--       [fulfillTx]
--       [injectFailure $ CollForPrecValidFailure (Coin 27000000)]

-- it "SWAPS-N: Collateral isn't sufficient to cover preceding transactions" $
--   do
--     st <- gets impNES
--     lEnv <- impLedgerEnv st
--     (_, addr1) <- freshKeyAddr
--     (_, addr2) <- freshKeyAddr
--     (_, addr3) <- freshKeyAddr
--     (_, addr4) <- freshKeyAddr
--     (_, addrCollatReq) <- freshKeyAddr
--     (_, addrCollatFf) <- freshKeyAddr

--     pure ()

-- rootTxIn <- fst <$> lookupImpRootTxOut

-- tx <-
--   submitTxAnn
--     "Sumbit a transaction with a script output"
--     $ mkBasicTx mkBasicTxBody
--       & bodyTxL
--       . outputsTxBodyL
--       .~ SSeq.fromList
--         [ ffTx addr1 445802143
--         , ffTx addr2 499911058
--         , ffTx addrCollatReq 27108750
--         , ffTx addrCollatFf 27006300
--         ]

-- let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
--     tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
--     txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
--     txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

-- let
--   requestTx' =
--     mkBasicTx mkBasicTxBody
--       & bodyTxL
--       . requestsTxBodyL
--       .~ SSeq.singleton (ffTx addr3 499731741)
--       & bodyTxL
--       . inputsTxBodyL
--       .~ Set.singleton tx1
--       & bodyTxL
--       . outputsTxBodyL
--       .~ SSeq.singleton (ffTx addr4 945353159)
--       & bodyTxL
--       . collateralInputsTxBodyL
--       .~ Set.singleton txCollatReq

-- let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
--     fulfillTx fulfills =
--       mkBasicTx mkBasicTxBody
--         & bodyTxL
--         . fulfillsTxBodyL
--         .~ fulfills
--         & bodyTxL
--         . inputsTxBodyL
--         .~ Set.singleton tx2
--         & bodyTxL
--         . collateralInputsTxBodyL
--         .~ Set.singleton txCollatFf
--         & isValidTxL
--         .~ IsValid False

-- void $
--   submitTxAnnZone
--     0
--     "Submit a transaction that consumes the script output"
--     [requestTx']
--     [Just fulfillTx]
--     [injectFailure $ CollForPrecValidFailure (Coin 27000000)]

ffTx ::
  (EraTxOut era, Value era ~ MaryValue (EraCrypto era), EraCrypto era ~ StandardCrypto) =>
  Addr (EraCrypto era) ->
  Integer ->
  MultiAsset StandardCrypto ->
  TxOut era
ffTx addr' amt ma =
  mkBasicTxOut
    addr'
    (MaryValue (Coin amt) ma)

-- utxoSpec ::
--   SpecWith (ImpTestState (BabelEra StandardCrypto))
-- utxoSpec = describe "UTXO" $ do
--   it "fails with a fulfills not in FRxO error" $ do
--     st <- gets impNES
--     lEnv <- impLedgerEnv st
--     (_, addr1) <- freshKeyAddr
--     (_, addr2) <- freshKeyAddr
--     (_, addr3) <- freshKeyAddr
--     (_, addr4) <- freshKeyAddr
--     (_, addrCollatReq) <- freshKeyAddr
--     (_, addrCollatFf) <- freshKeyAddr

--     pure ()

-- rootTxIn <- fst <$> lookupImpRootTxOut

-- tx <-
--   submitTxAnn
--     "Sumbit a transaction with a script output"
--     $ mkBasicTx mkBasicTxBody
--       & bodyTxL
--       . outputsTxBodyL
--       .~ SSeq.fromList
--         [ ffTx addr1 418562959
--         , ffTx addr2 499912642
--         , ffTx addrCollatReq 27108750
--         , ffTx addrCollatFf 54243900
--         ]

-- let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
--     tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
--     txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
--     txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx

-- let
--   requestTx' =
--     mkBasicTx mkBasicTxBody
--       & bodyTxL
--       . requestsTxBodyL
--       .~ SSeq.singleton (ffTx addr3 499731741)
--       & bodyTxL
--       . inputsTxBodyL
--       .~ Set.singleton tx1
--       & bodyTxL
--       . outputsTxBodyL
--       .~ SSeq.singleton (ffTx addr4 918113975)
--       & bodyTxL
--       . collateralInputsTxBodyL
--       .~ Set.singleton txCollatReq

-- let fulfillTx :: Set.Set (Fulfill (EraCrypto (BabelEra StandardCrypto))) -> Tx (BabelEra StandardCrypto)
--     fulfillTx fulfills =
--       mkBasicTx mkBasicTxBody
--         & bodyTxL
--         . fulfillsTxBodyL
--         .~ fulfills
--         & bodyTxL
--         . inputsTxBodyL
--         .~ Set.singleton tx2
--         & bodyTxL
--         . collateralInputsTxBodyL
--         .~ Set.singleton txCollatFf

-- let badFulfill = Set.singleton (txInAt (10 :: Int) requestTx')

-- submitTxAnnZone
--   0
--   "Submit a transaction that consumes the script output"
--   [requestTx']
--   [Just ((bodyTxL . fulfillsTxBodyL <>~ badFulfill) . fulfillTx)]
--   [injectFailure $ BadFulfillsFRxO badFulfill]
-- it "fails with ValueNotConservedUTxO due to outputs" $ do
--   (_, addr1) <- freshKeyAddr
--   (requestTx, fulfillTx) <- makeTestTransactions

--   pure ()

-- submitTxAnnZone
--   0
--   "Submit a transaction that consumes the script output"
--   [requestTx & bodyTxL . outputsTxBodyL <>~ SSeq.singleton (ffTx addr1 1)]
--   [Just fulfillTx]
--   [ injectFailure $ ValueNotConservedUTxO (Val.inject (Coin 918533884)) (Val.inject (Coin 918535337))
--   , injectFailure $ CollForPrecValidFailure (Coin 217800)
--   ]
-- it "fails with ValueNotConservedUTxO due to requests" $ do
--   (_, addr1) <- freshKeyAddr
--   (requestTx, fulfillTx) <- makeTestTransactions

--   pure ()

-- submitTxAnnZone
--   0
--   "Submit a transaction that consumes the script output"
--   [requestTx & bodyTxL . requestsTxBodyL <>~ SSeq.singleton (ffTx addr1 1)]
--   [Just fulfillTx]
--   [ injectFailure $ ValueNotConservedUTxO (Val.inject (Coin 918533885)) (Val.inject (Coin 918535336))
--   , injectFailure $ CollForPrecValidFailure (Coin 217800)
--   ]

-- TODO WG add witness test if possible once we're sure the witness logic is correct

makeTestTransactions ::
  ImpTestM
    (BabelEra StandardCrypto)
    ( Tx (BabelEra StandardCrypto)
    , BabelTx (BabelEra StandardCrypto)
    )
makeTestTransactions = do
  st <- gets impNES
  lEnv <- impLedgerEnv st
  (_, addr1) <- freshKeyAddr
  (_, addr2) <- freshKeyAddr
  (_, addr3) <- freshKeyAddr
  (_, addr4) <- freshKeyAddr
  (_, addrMa) <- freshKeyAddr
  (_, addrSink1) <- freshKeyAddr
  (_, addrSink2) <- freshKeyAddr
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
          [ ffTx addr1 419211201 mempty
          , ffTx addr2 499550884 mempty
          , ffTx addrCollatReq 27108750 mempty
          , ffTx addrCollatFf 54012900 mempty
          , ffTx addrMa 0 smallValue
          , ffTx addrSink1 0 smallValue
          , ffTx addrSink2 0 largeValue
          ]

  let tx1 = txInAt @Int @(BabelEra StandardCrypto) (0 :: Int) tx
      tx2 = txInAt @Int @(BabelEra StandardCrypto) (1 :: Int) tx
      txCollatReq = txInAt @Int @(BabelEra StandardCrypto) (2 :: Int) tx
      txCollatFf = txInAt @Int @(BabelEra StandardCrypto) (3 :: Int) tx
      txMa = txInAt @Int @(BabelEra StandardCrypto) (4 :: Int) tx

  let
    requestTx' =
      mkBasicTx mkBasicTxBody
        & bodyTxL
        . inputsTxBodyL
        .~ Set.singleton txMa
        & bodyTxL
        . outputsTxBodyL
        .~ SSeq.singleton (ffTx addr4 419162229 mempty)
        & bodyTxL
        . collateralInputsTxBodyL
        .~ Set.singleton txCollatReq

  let fulfillTx :: Tx (BabelEra StandardCrypto)
      fulfillTx =
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . inputsTxBodyL
          .~ Set.singleton tx1
          & bodyTxL
          . collateralInputsTxBodyL
          .~ Set.singleton txCollatFf
          & bodyTxL
          . outputsTxBodyL
          .~ SSeq.singleton (ffTx addr3 0 smallValue)

  pure (requestTx', fulfillTx)

-- --

-- -- | Alice's payment key pair
-- alicePay :: KeyPair 'Payment StandardCrypto
-- alicePay = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

-- -- | Alice's stake key pair
-- aliceStake :: KeyPair 'Staking StandardCrypto
-- aliceStake = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

-- -- | Alice's base address
-- aliceAddr :: Addr StandardCrypto
-- aliceAddr = mkAddr (alicePay, aliceStake)

-- -- | Bob's payment key pair
-- bobPay :: KeyPair 'Payment StandardCrypto
-- bobPay = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 2 2 2 2 2)

-- -- | Bob's stake key pair
-- bobStake :: KeyPair 'Staking StandardCrypto
-- bobStake = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 3 3 3 3 3)

-- -- | Bob's address
-- bobAddr :: Addr StandardCrypto
-- bobAddr = mkAddr (bobPay, bobStake)

-- -- Carl's payment key pair
-- carlPay :: KeyPair 'Payment StandardCrypto
-- carlPay = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 4 4 4 4 4)

-- -- | Carl's stake key pair
-- carlStake :: KeyPair 'Staking StandardCrypto
-- carlStake = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 5 5 5 5 5)

-- -- | Carl's address
-- carlAddr :: Addr StandardCrypto
-- carlAddr = mkAddr (carlPay, carlStake)

-- -- | Daria's payment key pair
-- dariaPay :: KeyPair 'Payment StandardCrypto
-- dariaPay = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 6 6 6 6 6)

-- -- | Daria's stake key pair
-- dariaStake :: KeyPair 'Staking StandardCrypto
-- dariaStake = KeyPair vk sk
--   where
--     (sk, vk) = mkKeyPair (RawSeed 7 7 7 7 7)

-- -- | Daria's address
-- dariaAddr :: Addr StandardCrypto
-- dariaAddr = mkAddr (dariaPay, dariaStake)
