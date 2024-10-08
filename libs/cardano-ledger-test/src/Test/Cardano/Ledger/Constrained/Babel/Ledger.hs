{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Constrained.Babel.Ledger where

import Cardano.Ledger.Shelley.API.Types

import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Rules (
  BabelUtxoEnv (..),
  getSubTxs,
  mkBatchData,
 )
import Cardano.Ledger.Babel.Tx (BabelTx)
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained.Base (runTerm)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Constrained.Babel.Instances
import Test.Cardano.Ledger.Constrained.Babel.Utxo

ledgerTxSpec ::
  forall fn.
  (IsConwayUniv fn, HasSpec fn (BabelTx (BabelEra StandardCrypto))) =>
  LedgerEnv (BabelEra StandardCrypto) ->
  LedgerState (BabelEra StandardCrypto) ->
  Specification fn (Tx (BabelEra StandardCrypto))
ledgerTxSpec env st =
  constrained $ \(tx :: Term fn (Tx (BabelEra StandardCrypto))) ->
    let tx' :: BabelTx (BabelEra StandardCrypto) -- TODO WG this looks very hacky, but I may not have time to fix
        tx' = case runTerm mempty tx of
          Result _ x -> x
          _ -> error ""
        subTxs = getSubTxs tx'
        parentTxId = txIdTx tx'
        batchData = mkBatchData tx' subTxs parentTxId
        observers = tx' ^. bodyTxL . requireBatchObserversTxBodyL
     in [ satisfies tx (utxoTxSpec (utxoEnv observers batchData) (lsUTxOState st))
        ]
  where
    utxoEnv observers batchData =
      BabelUtxoEnv
        { bueSlot = ledgerSlotNo env
        , buePParams = ledgerPp env
        , bueCertState = lsCertState st
        , bueRequiredBatchObservers = observers
        , bueBatchData = batchData
        }
