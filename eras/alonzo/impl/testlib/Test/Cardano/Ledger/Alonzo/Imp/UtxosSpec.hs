{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Core (
  AlonzoEraScript (..),
  AlonzoEraTx (..),
  AlonzoEraTxWits (..),
  AsIx (AsIx),
  ppMaxTxExUnitsL,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language, hashPlutusScript, withSLanguage)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest (
  ImpTestState,
  ShelleyEraImp,
  getsNES,
  produceScript,
  submitTxAnn_,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus.Examples (redeemerSameAsDatum)

spec ::
  forall era.
  ( ShelleyEraImp era
  , AlonzoEraTx era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOS" $
  forM_ ([minBound .. eraMaxLanguage @era] :: [Language]) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let scriptHash = hashPlutusScript (redeemerSameAsDatum slang)
        it "Spending script with a Datum" $ do
          txIn0 <- produceScript scriptHash
          submitTxAnn_ "Submit a transaction that consumes the script output" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL
                .~ Set.singleton txIn0
        it "Invalid plutus script fails in phase 2" $ do
          txIn0 <- produceScript scriptHash
          exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
          submitTxAnn_ "Submitting consuming transaction" $
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0
                & isValidTxL .~ IsValid False
                & witsTxL . rdmrsTxWitsL
                  .~ Redeemers
                    ( Map.singleton
                        (mkSpendingPurpose $ AsIx 0)
                        (Data $ P.I 32, exUnits)
                    )
            )
