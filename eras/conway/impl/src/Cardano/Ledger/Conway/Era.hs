{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayLedgerState (..),
  ConwayCERT,
  ConwayDELEG,
  ConwayGOVCERT,
  ConwayCERTS,
  ConwayGOV,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACT,
  ConwayUTXO,
  ConwayUTXOS,
  ConwayUTXOW,
  ConwayTICKF,
  ConwayLEDGER,
  ConwayRATIFY,
) where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (EraGov (..))
import Cardano.Ledger.Binary (EncCBOR, decNoShareCBOR)
import Cardano.Ledger.Binary.Decoding (DecCBOR (..), DecShareCBOR (..), Interns)
import Cardano.Ledger.Binary.Encoding (EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.LedgerState (
  HasLedgerState (..),
  LedgerState,
  lsCertStateL,
  lsUTxOStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOL,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )
import Control.DeepSeq (NFData)
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import Lens.Micro (lens)
import Lens.Micro.Type (Lens')
import NoThunks.Class (NoThunks)

-- =====================================================

-- | The Conway era
data ConwayEra c

instance Crypto c => Era (ConwayEra c) where
  type PreviousEra (ConwayEra c) = BabbageEra c
  type EraCrypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 9
  type ProtVerHigh (ConwayEra c) = 10

  eraName = "Conway"

type instance Value (ConwayEra c) = MaryValue c

newtype ConwayLedgerState era = ConwayLedgerState {unConwayLedgerState :: LedgerState era}
  deriving (Generic, Default)

conwayLedgerStateL :: Lens' (ConwayLedgerState era) (LedgerState era)
conwayLedgerStateL = lens unConwayLedgerState (\x y -> x {unConwayLedgerState = y})

instance (Crypto c, EraGov (ConwayEra c), EraTxOut (ConwayEra c)) => HasLedgerState (ConwayEra c) where
  type EraLedgerState (ConwayEra c) = ConwayLedgerState (ConwayEra c)
  hlsUTxOStateL = conwayLedgerStateL . lsUTxOStateL
  hlsCertStateL = conwayLedgerStateL . lsCertStateL

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (ConwayLedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (ConwayLedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (ConwayLedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (ConwayLedgerState era)

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  EncCBOR (ConwayLedgerState era)
  where
  encCBOR (ConwayLedgerState ls) = encCBOR ls

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecCBOR (ConwayLedgerState era)
  where
  decCBOR = decNoShareCBOR

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (ConwayLedgerState era)
  where
  type
    Share (ConwayLedgerState era) =
      ( Interns (Credential 'Staking (EraCrypto era))
      , Interns (KeyHash 'StakePool (EraCrypto era))
      )
  decSharePlusCBOR = ConwayLedgerState <$> decSharePlusCBOR

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" (ConwayEra c) = VoidEraRule "UPEC" (ConwayEra c)
type instance EraRuleFailure "UPEC" (ConwayEra c) = VoidEraRule "UPEC" (ConwayEra c)
type instance EraRuleEvent "UPEC" (ConwayEra c) = VoidEraRule "UPEC" (ConwayEra c)

type instance EraRule "NEWPP" (ConwayEra c) = VoidEraRule "NEWPP" (ConwayEra c)
type instance EraRuleFailure "NEWPP" (ConwayEra c) = VoidEraRule "NEWPP" (ConwayEra c)
type instance EraRuleEvent "NEWPP" (ConwayEra c) = VoidEraRule "NEWPP" (ConwayEra c)

type instance EraRule "PPUP" (ConwayEra c) = VoidEraRule "PPUP" (ConwayEra c)
type instance EraRuleFailure "PPUP" (ConwayEra c) = VoidEraRule "PPUP" (ConwayEra c)
type instance EraRuleEvent "PPUP" (ConwayEra c) = VoidEraRule "PPUP" (ConwayEra c)

type instance EraRule "MIR" (ConwayEra c) = VoidEraRule "MIR" (ConwayEra c)
type instance EraRuleFailure "MIR" (ConwayEra c) = VoidEraRule "MIR" (ConwayEra c)
type instance EraRuleEvent "MIR" (ConwayEra c) = VoidEraRule "MIR" (ConwayEra c)

type instance EraRule "DELEGS" (ConwayEra c) = VoidEraRule "DELEGS" (ConwayEra c)
type instance EraRuleFailure "DELEGS" (ConwayEra c) = VoidEraRule "DELEGS" (ConwayEra c)
type instance EraRuleEvent "DELEGS" (ConwayEra c) = VoidEraRule "DELEGS" (ConwayEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data ConwayGOV era

type instance EraRule "GOV" (ConwayEra c) = ConwayGOV (ConwayEra c)

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" (ConwayEra c) = ConwayNEWEPOCH (ConwayEra c)

data ConwayEPOCH era

type instance EraRule "EPOCH" (ConwayEra c) = ConwayEPOCH (ConwayEra c)

data ConwayENACT era

type instance EraRule "ENACT" (ConwayEra c) = ConwayENACT (ConwayEra c)

data ConwayUTXOS era

type instance EraRule "UTXOS" (ConwayEra c) = ConwayUTXOS (ConwayEra c)

data ConwayLEDGER era

type instance EraRule "LEDGER" (ConwayEra c) = ConwayLEDGER (ConwayEra c)

data ConwayTICKF era

type instance EraRule "TICKF" (ConwayEra c) = ConwayTICKF (ConwayEra c)

data ConwayRATIFY era

type instance EraRule "RATIFY" (ConwayEra c) = ConwayRATIFY (ConwayEra c)

data ConwayCERTS era

type instance EraRule "CERTS" (ConwayEra c) = ConwayCERTS (ConwayEra c)

data ConwayCERT era

type instance EraRule "CERT" (ConwayEra c) = ConwayCERT (ConwayEra c)

data ConwayDELEG era

type instance EraRule "DELEG" (ConwayEra c) = ConwayDELEG (ConwayEra c)

data ConwayGOVCERT era

type instance EraRule "GOVCERT" (ConwayEra c) = ConwayGOVCERT (ConwayEra c)

data ConwayUTXOW era

type instance EraRule "UTXOW" (ConwayEra c) = ConwayUTXOW (ConwayEra c)

data ConwayUTXO era

type instance EraRule "UTXO" (ConwayEra c) = ConwayUTXO (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" (ConwayEra c) = API.ShelleyLEDGERS (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.ShelleyPOOLREAP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = ShelleyRUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = ShelleySNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = ShelleyTICK (ConwayEra c)

type instance EraRule "POOL" (ConwayEra c) = ShelleyPOOL (ConwayEra c)

-- =================================================
