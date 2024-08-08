{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel (
  Babel,
  BabelEra,
)
where

import Cardano.Crypto.DSIGN (Signable)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Governance ()
import Cardano.Ledger.Babel.Rules ()
import Cardano.Ledger.Babel.Rules.Gov ()
import Cardano.Ledger.Babel.Transition ()
import Cardano.Ledger.Babel.Translation ()
import Cardano.Ledger.Babel.Tx ()
import Cardano.Ledger.Babel.TxInfo ()
import Cardano.Ledger.Babel.TxOut ()
import Cardano.Ledger.Babel.UTxO ()
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Crypto (Crypto (DSIGN), StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.API (ApplyBlock, LedgerState)
import Cardano.Ledger.Shelley.API.Genesis (CanStartFromGenesis (..))
import Cardano.Ledger.Shelley.API.Mempool (
  ApplyTx (reapplyTx),
 )

import qualified Cardano.Crypto.Hash.Class
import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Transition ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxInfo ()
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.Babel.Scripts ()
import Data.Default.Class (Default)

type Babel = BabelEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , -- TODO WG figure out what you've done wrong to introduce this constraint
    Signable (DSIGN c) (Cardano.Crypto.Hash.Class.Hash c EraIndependentTxBody)
  , Default (LedgerState (BabelEra c))
  ) =>
  ApplyTx (BabelEra c)
  where
  reapplyTx = reapplyAlonzoTx

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , Signable (DSIGN c) (Cardano.Crypto.Hash.Class.Hash c EraIndependentTxBody)
  ) =>
  ApplyBlock (BabelEra c)

instance Crypto c => CanStartFromGenesis (BabelEra c) where
  type AdditionalGenesisConfig (BabelEra c) = BabelGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Babel to work"

instance Crypto c => RunConwayRatify (BabelEra c)
