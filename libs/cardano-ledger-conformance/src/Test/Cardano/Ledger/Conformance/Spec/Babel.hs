{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Babel (spec) where

import Test.Cardano.Ledger.Babel.ImpTest (withImpState)
import Test.Cardano.Ledger.Conformance (conformsToImpl)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Babel ()
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn)
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = describe "Babel conformance tests" $ do
  withImpState $ do
    xit "UTXO" . replicateM_ 100 $ conformsToImpl @"UTXO" @ConwayFn
    xit "GOV" . replicateM_ 100 $ conformsToImpl @"GOV" @ConwayFn
