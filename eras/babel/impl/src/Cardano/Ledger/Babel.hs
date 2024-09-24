{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- CanStartFromGenesis

module Cardano.Ledger.Babel (
  Babel,
  BabelEra,
)
where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Governance ()
import Cardano.Ledger.Babel.Rules ()
import Cardano.Ledger.Babel.Transition ()
import Cardano.Ledger.Babel.Translation ()
import Cardano.Ledger.Babel.TxInfo ()
import Cardano.Ledger.Babel.TxOut ()
import Cardano.Ledger.Babel.TxSeq (BabelTxSeq)
import Cardano.Ledger.Babel.UTxO ()
import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Core (EraIndependentTxBody, EraSegWits (TxSeq), EraTx (..))
import Cardano.Ledger.Crypto (Crypto (DSIGN), HASH, StandardCrypto)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.API (ApplyTx (reapplyTx), LedgerState)
import Cardano.Ledger.Shelley.API.Genesis (CanStartFromGenesis (..))
import Cardano.Ledger.Shelley.API.Validation (ApplyBlock)
import Data.Default.Class (Default)
import Data.Sequence

type Babel = BabelEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , Signable (DSIGN c) (Hash (Cardano.Ledger.Crypto.HASH c) EraIndependentTxBody)
  , Default (LedgerState (BabelEra c))
  ) =>
  ApplyTx (BabelEra c)
  where
  reapplyTx = reapplyAlonzoTx

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , Signable (DSIGN c) (Hash (Cardano.Ledger.Crypto.HASH c) EraIndependentTxBody)
  , TxSeq (BabelEra c) ~ BabelTxSeq (BabelEra c)
  , DecCBOR (Annotator (Seq (Tx (BabelEra c))))
  ) =>
  ApplyBlock (BabelEra c)

instance Crypto c => CanStartFromGenesis (BabelEra c) where
  type AdditionalGenesisConfig (BabelEra c) = BabelGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Babel to work"

instance Crypto c => RunConwayRatify (BabelEra c)
