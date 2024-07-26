{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.FRxO where

import Cardano.Ledger.Binary (
  DecCBOR,
  DecShareCBOR (Share),
  EncCBOR,
  FromCBOR,
  Interns,
  ToCBOR (toCBOR),
  decCBOR,
  decodeMap,
 )
import Cardano.Ledger.Binary.Decoding (DecShareCBOR (decShareCBOR))
import Cardano.Ledger.Binary.Plain (fromCBOR)
import Cardano.Ledger.Core (Era (EraCrypto), EraTxOut (TxOut), fromEraCBOR, toEraCBOR)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (UTxO (UTxO))
import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Aeson (ToJSON)
import Data.Default.Class (Default)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Quiet (Quiet (Quiet))

-- | The unspent transaction outputs.
newtype FRxO era = FRxO {unFRxO :: Map.Map (TxIn (EraCrypto era)) (TxOut era)}
  deriving (Default, Generic, Semigroup)

asUtxoWith :: (UTxO era -> b) -> FRxO era -> b
asUtxoWith f = f . UTxO . unFRxO

instance (EncCBOR (TxOut era), Era era) => ToCBOR (FRxO era) where
  toCBOR = toEraCBOR @era

instance (DecCBOR (TxOut era), Era era) => FromCBOR (FRxO era) where
  fromCBOR = fromEraCBOR @era

deriving instance NoThunks (TxOut era) => NoThunks (FRxO era)

deriving instance (Era era, NFData (TxOut era)) => NFData (FRxO era)

deriving newtype instance (Era era, Eq (TxOut era)) => Eq (FRxO era)

deriving newtype instance Era era => Monoid (FRxO era)

deriving newtype instance (Era era, EncCBOR (TxOut era)) => EncCBOR (FRxO era)

deriving newtype instance (Era era, DecCBOR (TxOut era)) => DecCBOR (FRxO era)

instance
  ( Crypto (EraCrypto era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  ) =>
  DecShareCBOR (FRxO era)
  where
  type
    Share (FRxO era) =
      Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credsInterns =
    FRxO <$!> decodeMap decCBOR (decShareCBOR credsInterns)

deriving via
  Quiet (FRxO era)
  instance
    (Show (TxOut era), Crypto (EraCrypto era)) => Show (FRxO era)

deriving newtype instance (Era era, ToJSON (TxOut era)) => ToJSON (FRxO era)

-- class EraUTxO era => EraFRxO era where
--   -- | Calculate all the value that is being consumed by the transaction.
--   getConsumedValueFrxo ::
--     PParams era ->
--     -- | Function that can lookup current delegation deposits
--     (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
--     -- | Function that can lookup current drep deposits
--     (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
--     FRxO era ->
--     TxBody era ->
--     Value era

--   getProducedValueFrxo ::
--     PParams era ->
--     -- | Check whether a pool with a supplied PoolStakeId is already registered.
--     (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
--     TxBody era ->
--     Value era

--   -- | Initial eras will look into witness set to find all of the available scripts, but
--   -- starting with Babbage we can look for available scripts in the UTxO using reference
--   -- inputs.
--   getScriptsProvidedFrxo ::
--     -- | For some era it is necessary to look into the UTxO to find all of the available
--     -- scripts for the transaction
--     FRxO era ->
--     Tx era ->
--     ScriptsProvided era

--   -- | Produce all the information required for figuring out which scripts are required
--   -- for the transaction to be valid, once those scripts are evaluated
--   getScriptsNeededFrxo :: FRxO era -> TxBody era -> ScriptsNeeded era

--   -- | Extract the set of all script hashes that are needed for script validation.
--   getScriptsHashesNeededFrxo :: ScriptsNeeded era -> Set (ScriptHash (EraCrypto era))

--   -- | Extract all of the KeyHash witnesses that are required for validating the transaction
--   getWitsVKeyNeededFrxo ::
--     CertState era -> UTxO era -> TxBody era -> Set (KeyHash 'Witness (EraCrypto era))

--   -- | Minimum fee computation, excluding witnesses and including ref scripts size
--   getMinFeeTxFrxo :: PParams era -> Tx era -> UTxO era -> Coin
