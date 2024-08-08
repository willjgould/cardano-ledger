{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Cardano.Ledger.Shelley.Imp (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp, withImpState)
import qualified Test.Cardano.Ledger.Shelley.UnitTests.IncrementalStakeTest as Incremental

spec ::
  forall era.
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  describe "ShelleyImpSpec" $ pure ()

-- withImpState @ls @era $ do
--   Ledger.spec @era
--   Epoch.spec @era
--   Utxow.spec @ls @era
--   Utxo.spec @ls @era
-- describe "ShelleyPureTests" $ do
--   Incremental.spec @era
