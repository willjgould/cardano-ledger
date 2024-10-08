{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERT rule
module Test.Cardano.Ledger.Constrained.Babel.Cert where

import Cardano.Ledger.CertState
import Cardano.Ledger.Shelley.API.Types
import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Conway.Rules (CertEnv (..), ConwayGovCertEnv (ConwayGovCertEnv))
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Babel.Deleg
import Test.Cardano.Ledger.Constrained.Babel.GovCert (govCertSpec, vStateSpec)
import Test.Cardano.Ledger.Constrained.Babel.Instances
import Test.Cardano.Ledger.Constrained.Babel.PParams
import Test.Cardano.Ledger.Constrained.Babel.Pool

certEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (CertEnv (BabelEra StandardCrypto))
certEnvSpec =
  constrained $ \ce ->
    match ce $ \_ pp _ ->
      satisfies pp pparamsSpec

certStateSpec ::
  IsConwayUniv fn =>
  Specification fn (CertState (BabelEra StandardCrypto))
certStateSpec =
  constrained $ \cs ->
    match cs $ \vState pState dState ->
      [ satisfies vState vStateSpec
      , satisfies pState pStateSpec
      , satisfies dState dStateSpec
      ]

txCertSpec ::
  IsConwayUniv fn =>
  CertEnv (BabelEra StandardCrypto) ->
  CertState (BabelEra StandardCrypto) ->
  Specification fn (ConwayTxCert (BabelEra StandardCrypto))
txCertSpec (CertEnv slot pp ce) CertState {..} =
  constrained $ \txCert ->
    caseOn
      txCert
      (branch $ \delegCert -> satisfies delegCert $ delegCertSpec pp certDState)
      (branch $ \poolCert -> satisfies poolCert $ poolCertSpec (PoolEnv slot pp) certPState)
      (branch $ \govCert -> satisfies govCert $ govCertSpec (ConwayGovCertEnv pp ce) certVState)

govCertEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayGovCertEnv (BabelEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ ->
      satisfies pp pparamsSpec