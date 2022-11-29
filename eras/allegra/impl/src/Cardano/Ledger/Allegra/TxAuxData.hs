{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxAuxData
  ( AllegraTxAuxData (AllegraTxAuxData, AllegraTxAuxData', ..),
    Core.TxAuxData,

    -- * Deprecations
    AuxiliaryData,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Allegra.Timelocks ()
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Binary (Annotator (..), FromCBOR (..), ToCBOR (..), peekTokenType)
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
  ( Era (..),
    EraTxAuxData (hashTxAuxData, validateTxAuxData),
    Script,
  )
import qualified Cardano.Ledger.Core as Core (TxAuxData)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.MemoBytes
  ( Mem,
    MemoBytes,
    MemoHashIndex,
    Memoized (RawType),
    getMemoRawType,
    getMemoSafeHash,
    mkMemoized,
  )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash, hashAnnotated)
import Cardano.Ledger.Shelley.Metadata (Metadatum, validMetadatum)
import Codec.CBOR.Decoding
  ( TokenType
      ( TypeListLen,
        TypeListLen64,
        TypeListLenIndef,
        TypeMapLen,
        TypeMapLen64,
        TypeMapLenIndef
      ),
  )
import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- =======================================

-- | Raw, un-memoised metadata type
data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { -- | Structured transaction metadata
    txMetadata :: !(Map Word64 Metadatum),
    -- | Pre-images of script hashes found within the TxBody, but which are not
    -- required as witnesses. Examples include:
    -- - Token policy IDs appearing in transaction outputs
    -- - Pool reward account registrations
    auxiliaryScripts :: !(StrictSeq (Script era))
  }
  deriving (Generic)

instance (Crypto c) => EraTxAuxData (AllegraEra c) where
  type TxAuxData (AllegraEra c) = AllegraTxAuxData (AllegraEra c)
  validateTxAuxData _ (AllegraTxAuxData md as) = as `deepseq` all validMetadatum md
  hashTxAuxData aux = AuxiliaryDataHash (hashAnnotated aux)

deriving instance Eq (Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Script era) => Show (AuxiliaryDataRaw era)

deriving instance NoThunks (Script era) => NoThunks (AuxiliaryDataRaw era)

instance NFData (Script era) => NFData (AuxiliaryDataRaw era)

newtype AllegraTxAuxData era = AuxiliaryDataWithBytes (MemoBytes AuxiliaryDataRaw era)
  deriving (Generic)
  deriving newtype (ToCBOR, SafeToHash)

instance Memoized AllegraTxAuxData where
  type RawType AllegraTxAuxData = AuxiliaryDataRaw

type instance MemoHashIndex AuxiliaryDataRaw = EraIndependentTxAuxData

instance (c ~ EraCrypto era) => HashAnnotated (AllegraTxAuxData era) EraIndependentTxAuxData c where
  hashAnnotated = getMemoSafeHash

deriving newtype instance Eq (Script era) => Eq (AllegraTxAuxData era)

deriving newtype instance
  (Show (Script era), HashAlgorithm (HASH (EraCrypto era))) =>
  Show (AllegraTxAuxData era)

deriving newtype instance
  (NoThunks (Script era), Era era) =>
  NoThunks (AllegraTxAuxData era)

deriving newtype instance NFData (Script era) => NFData (AllegraTxAuxData era)

pattern AllegraTxAuxData ::
  (ToCBOR (Script era), Era era) =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData blob sp <- (getMemoRawType -> AuxiliaryDataRaw blob sp)
  where
    AllegraTxAuxData blob sp = mkMemoized $ AuxiliaryDataRaw blob sp

{-# COMPLETE AllegraTxAuxData #-}

type AuxiliaryData = AllegraTxAuxData

{-# DEPRECATED AuxiliaryData "Use `AllegraTxAuxData` instead" #-}

pattern AllegraTxAuxData' ::
  Era era =>
  Map Word64 Metadatum ->
  StrictSeq (Script era) ->
  AllegraTxAuxData era
pattern AllegraTxAuxData' blob sp <-
  (getMemoRawType -> AuxiliaryDataRaw blob sp)

{-# COMPLETE AllegraTxAuxData' #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance (Era era, ToCBOR (Script era)) => ToCBOR (AuxiliaryDataRaw era) where
  toCBOR (AuxiliaryDataRaw blob sp) =
    encode (Rec AuxiliaryDataRaw !> To blob !> To sp)

instance
  (Era era, FromCBOR (Annotator (Script era))) =>
  FromCBOR (Annotator (AuxiliaryDataRaw era))
  where
  fromCBOR =
    peekTokenType >>= \case
      TypeMapLen -> decodeFromMap
      TypeMapLen64 -> decodeFromMap
      TypeMapLenIndef -> decodeFromMap
      TypeListLen -> decodeFromList
      TypeListLen64 -> decodeFromList
      TypeListLenIndef -> decodeFromList
      _ -> error "Failed to decode AuxiliaryData"
    where
      decodeFromMap =
        decode
          ( Ann (Emit AuxiliaryDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
          )
      decodeFromList =
        decode
          ( Ann (RecD AuxiliaryDataRaw)
              <*! Ann From
              <*! D (sequence <$> fromCBOR)
          )

deriving via
  (Mem AuxiliaryDataRaw era)
  instance
    (Era era, FromCBOR (Annotator (Script era))) =>
    FromCBOR (Annotator (AllegraTxAuxData era))
