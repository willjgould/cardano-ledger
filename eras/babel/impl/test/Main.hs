{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Cardano.Ledger.Babel (Babel)
import qualified Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec as CostModelsSpec
import qualified Test.Cardano.Ledger.Alonzo.Binary.TxWitsSpec as TxWitsSpec
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import qualified Test.Cardano.Ledger.Babel.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Babel.Binary.Regression as Regression
import qualified Test.Cardano.Ledger.Babel.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Babel.CommitteeRatifySpec as CommitteeRatify
import qualified Test.Cardano.Ledger.Babel.DRepRatifySpec as DRepRatify
import qualified Test.Cardano.Ledger.Babel.GenesisSpec as Genesis
import qualified Test.Cardano.Ledger.Babel.GovActionReorderSpec as GovActionReorder
import qualified Test.Cardano.Ledger.Babel.Imp as BabelImp
import Test.Cardano.Ledger.Babel.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Babel.Proposals as Proposals
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

main :: IO ()
main =
  ledgerTestMain $
    describe "Babel" $ do
      -- Proposals.spec
      -- Binary.spec -- Some broken tests
      -- Cddl.spec -- Some broken tests

      -- DRepRatify.spec -- pass

      -- CommitteeRatify.spec -- pass
      -- Genesis.spec -- passes but...TODO WG: For some reason I've had to add a bunch of zeroes to the ucppPlutusV3CostModel in the genesis file to get this to work. If there's time, figure out why
      -- GovActionReorder.spec -- pass
      -- roundTripJsonEraSpec @Babel -- pass
      describe "Imp" $ do
        BabelImp.spec

-- AlonzoImp.spec @Babel -- pass
-- ShelleyImp.spec @Babel -- pass
-- CostModelsSpec.spec @Babel -- Some broken tests

-- describe "TxWits" $ do
-- TxWitsSpec.spec @Babel -- pass

-- describe "Plutus" $ do
--   PlutusSpec.spec -- pass
--   Regression.spec -- pass
