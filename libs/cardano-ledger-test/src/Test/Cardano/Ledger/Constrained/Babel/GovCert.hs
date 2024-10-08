{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOVCERT rule
module Test.Cardano.Ledger.Constrained.Babel.GovCert where

import Cardano.Ledger.CertState

import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Conway.PParams (ppDRepDepositL)
import Cardano.Ledger.Conway.Rules (ConwayGovCertEnv (..))
import Cardano.Ledger.Conway.TxCert (ConwayGovCert)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Data.Map as Map
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Constrained.Babel.Instances
import Test.Cardano.Ledger.Constrained.Babel.PParams

vStateSpec :: Specification fn (VState (BabelEra StandardCrypto))
vStateSpec = TrueSpec

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (BabelEra StandardCrypto) ->
  VState (BabelEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} vs =
  let reps = lit $ Map.keysSet $ vsDReps vs
      deposits = lit [(k, drepDeposit dep) | (k, dep) <- Map.toList $ vsDReps vs]
   in constrained $ \gc ->
        caseOn
          gc
          -- ConwayRegDRep
          ( branch $ \key coin _ ->
              [ not_ $ member_ key reps
              , coin ==. lit (cgcePParams ^. ppDRepDepositL)
              ]
          )
          -- ConwayUnRegDRep
          ( branch $ \cred coin ->
              elem_ (pair_ cred coin) deposits
          )
          -- ConwayUpdateDRep
          ( branch $ \key _ ->
              member_ key reps
          )
          -- ConwayAuthCommitteeHotKey
          (branch $ \_ _ -> True)
          -- ConwayResignCommitteeColdKey
          (branch $ \_ _ -> True)

govCertEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayGovCertEnv (BabelEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ ->
      satisfies pp pparamsSpec
