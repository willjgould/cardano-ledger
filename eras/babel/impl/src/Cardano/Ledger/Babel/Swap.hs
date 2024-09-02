{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Swap where

import Cardano.Ledger.Alonzo.Core (AlonzoEraScript)
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.TxAuxData ()
import Cardano.Ledger.Babel.TxBody (BabelEraTxBody)
import Cardano.Ledger.Babel.TxWits ()
import Cardano.Ledger.BaseTypes (
  StrictMaybe (..),
  maybeToStrictMaybe,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR (..),
  allowTag,
  decodeNullMaybe,
  ifDecoderVersionAtLeast,
  natVersion,
  setTag,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Binary.Decoding (
  decodeList,
  decodeMapTraverse,
  decodeNonEmptyList,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyRole (..), WitVKey)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.TxWits (mapTraverseableDecoderA)
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData (..))
import Data.Data (Typeable)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Strict
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

data SwapRaw era = SwapRaw
  { stxrTxBody :: TxBody era
  , stxrWits :: !(Set (WitVKey 'Witness (EraCrypto era)))
  , stxrDats :: !(TxDats era)
  , stxrRdmrs :: !(Redeemers era)
  , stxrAux :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic, Typeable)

instance (AlonzoEraScript era, EqRaw (TxBody era), Eq (TxAuxData era)) => EqRaw (SwapRaw era) where
  eqRaw = babelEqSwapRaw

instance Crypto c => DecCBOR (Annotator (TxId c)) where
  decCBOR = pure <$> decCBOR

class BabelEraTxSwap era where
  datsTxSwapL :: Lens' (TxSwap era) (TxDats era)
  redeemersTxSwapL :: Lens' (TxSwap era) (Redeemers era)

instance (Crypto c, BabelEraTxBody (BabelEra c)) => BabelEraTxSwap (BabelEra c) where
  datsTxSwapL = lensMemoRawType stxrDats (\swapRaw dats_ -> swapRaw {stxrDats = dats_})
  redeemersTxSwapL = lensMemoRawType stxrRdmrs (\swapRaw rdmrs_ -> swapRaw {stxrRdmrs = rdmrs_})

datsBabelTxSwapL :: Lens' (SwapRaw era) (TxDats era)
datsBabelTxSwapL = lens stxrDats setter
  where
    setter (SwapRaw a b _ d e) n = SwapRaw a b n d e

redeemersBabelTxSwapL :: Lens' (SwapRaw era) (Redeemers era)
redeemersBabelTxSwapL = lens stxrRdmrs setter
  where
    setter (SwapRaw a b c _ e) n = SwapRaw a b c n e

type instance MemoHashIndex SwapRaw = EraIndependentSwap

instance c ~ EraCrypto era => HashAnnotated (Swap era) EraIndependentSwap c where
  hashAnnotated = getMemoSafeHash

emptySwapRaw :: (EraTxBody era, AlonzoEraScript era) => SwapRaw era
emptySwapRaw =
  SwapRaw
    mkBasicTxBody
    mempty
    mempty
    mempty
    SNothing

instance
  (Eq (TxBody era), Eq (TxAuxData era), AlonzoEraScript era) =>
  EqRaw (Swap era)

instance
  (Crypto c, BabelEraTxBody (BabelEra c)) =>
  EraTxSwap (BabelEra c)
  where
  type TxSwap (BabelEra c) = Swap (BabelEra c)

  mkBasicTxSwap :: Swap (BabelEra c)
  mkBasicTxSwap = mkMemoized emptySwapRaw

  txBodyTxSwapL :: Lens' (Swap (BabelEra c)) (TxBody (BabelEra c))
  txBodyTxSwapL = lensMemoRawType stxrTxBody (\swapRaw txBody_ -> swapRaw {stxrTxBody = txBody_})
  {-# INLINEABLE txBodyTxSwapL #-}

  witnessesTxSwapL :: Lens' (Swap (BabelEra c)) (Set (WitVKey 'Witness (EraCrypto (BabelEra c))))
  witnessesTxSwapL = lensMemoRawType stxrWits (\txBodyRaw wits_ -> txBodyRaw {stxrWits = wits_})
  {-# INLINEABLE witnessesTxSwapL #-}

  auxDataTxSwapL :: Lens' (Swap (BabelEra c)) (StrictMaybe (TxAuxData (BabelEra c)))
  auxDataTxSwapL = lensMemoRawType stxrAux (\swapRaw aux_ -> swapRaw {stxrAux = aux_})
  {-# INLINEABLE auxDataTxSwapL #-}

newtype BabelTxSwapsRaw era = BabelTxSwapsRaw (Strict.Map (TxId (EraCrypto era)) (Swap era))
  deriving (Generic, Semigroup, Monoid, EncCBOR)

newtype BabelTxSwaps era = BabelTxSwapsConstr (MemoBytes BabelTxSwapsRaw era)
  deriving newtype (SafeToHash, ToCBOR)
  deriving (Generic)

instance Era era => EncCBOR (BabelTxSwaps era)

deriving via
  (Mem BabelTxSwapsRaw era)
  instance
    (AlonzoEraScript era, EraTxBody era, DecCBOR (Annotator (TxAuxData era))) =>
    DecCBOR (Annotator (BabelTxSwaps era))

instance
  (AlonzoEraScript era, EraTxBody era, DecCBOR (Annotator (TxAuxData era))) =>
  DecCBOR (Annotator (BabelTxSwapsRaw era))
  where
  decCBOR =
    decode $
      Ann (RecD BabelTxSwapsRaw)
        <*! D (decodeMapTraverse decCBOR decCBOR)
  {-# INLINE decCBOR #-}

instance Memoized BabelTxSwaps where
  type RawType BabelTxSwaps = BabelTxSwapsRaw

deriving stock instance
  (AlonzoEraScript era, Eq (TxBody era), Eq (TxAuxData era)) =>
  Eq (BabelTxSwapsRaw era)

deriving stock instance
  (AlonzoEraScript era, Show (TxBody era), Show (TxAuxData era)) =>
  Show (BabelTxSwapsRaw era)

instance
  (AlonzoEraScript era, NoThunks (TxBody era), NoThunks (TxAuxData era)) =>
  NoThunks (BabelTxSwapsRaw era)

deriving newtype instance
  (AlonzoEraScript era, Eq (TxBody era), Eq (TxAuxData era)) =>
  Eq (BabelTxSwaps era)

deriving newtype instance
  (AlonzoEraScript era, Show (TxBody era), Show (TxAuxData era)) =>
  Show (BabelTxSwaps era)

deriving newtype instance
  (AlonzoEraScript era, NoThunks (TxBody era), NoThunks (TxAuxData era)) =>
  NoThunks (BabelTxSwaps era)

instance
  ( Crypto c
  , BabelEraTxBody (BabelEra c)
  , EncCBOR (BabelTxSwaps (BabelEra c))
  , DecCBOR (Annotator (BabelTxSwaps (BabelEra c)))
  , EqRaw (BabelTxSwaps (BabelEra c))
  , HashAnnotated (BabelTxSwaps (BabelEra c)) EraIndependentSwaps c
  , Monoid (BabelTxSwaps (BabelEra c))
  , NFData (BabelTxSwaps (BabelEra c))
  ) =>
  EraTxSwaps (BabelEra c)
  where
  type TxSwaps (BabelEra c) = BabelTxSwaps (BabelEra c)

  mkBasicTxSwaps :: TxSwaps (BabelEra c)
  mkBasicTxSwaps = mkMemoized mempty
  fromTxSwaps (BabelTxSwaps swaps) = swaps
  toTxSwaps = BabelTxSwaps

txBodyBabelTxSwapL :: Lens' (SwapRaw era) (TxBody era)
txBodyBabelTxSwapL = lens stxrTxBody setter
  where
    setter (SwapRaw _ b c d e) n = SwapRaw n b c d e

witnessesBabelTxSwapL :: Lens' (SwapRaw era) (Set (WitVKey 'Witness (EraCrypto era)))
witnessesBabelTxSwapL = lens stxrWits setter
  where
    setter (SwapRaw a _ c d e) n = SwapRaw a n c d e

auxBabelTxSwapL :: Lens' (SwapRaw era) (StrictMaybe (TxAuxData era))
auxBabelTxSwapL = lens stxrAux setter
  where
    setter (SwapRaw a b c d _) = SwapRaw a b c d

-- babelEqRawSwapRaw ::
--   ( AlonzoEraScript era
--   , EqRaw (TxBody era)
--   , Eq (TxAuxData era)
--   ) =>
--   Swap era ->
--   Swap era ->
--   Bool
-- babelEqRawSwapRaw tx1 tx2 =
--   stxTxBody tx1 `eqRaw` stxTxBody tx2
--     && stxWits tx1 == stxWits tx2
--     && stxDats tx1 == stxDats tx2
--     && stxRdmrs tx1 == stxRdmrs tx2
--     && stxAux tx1 == stxAux tx2

babelEqSwapRaw ::
  ( AlonzoEraScript era
  , EqRaw (TxBody era)
  , Eq (TxAuxData era)
  ) =>
  SwapRaw era ->
  SwapRaw era ->
  Bool
babelEqSwapRaw tx1 tx2 =
  stxrTxBody tx1 `eqRaw` stxrTxBody tx2
    && stxrWits tx1 == stxrWits tx2
    && stxrDats tx1 == stxrDats tx2
    && stxrRdmrs tx1 == stxrRdmrs tx2
    && stxrAux tx1 == stxrAux tx2

newtype Swap era = SwapConstr (MemoBytes SwapRaw era)
  deriving (ToCBOR, Generic)
  deriving newtype (SafeToHash)

instance Memoized Swap where
  type RawType Swap = SwapRaw

deriving instance (Eq (TxBody era), Eq (TxAuxData era), AlonzoEraScript era) => Eq (SwapRaw era)

instance
  ( AlonzoEraScript era
  , NoThunks (TxBody era)
  , NoThunks (TxAuxData era)
  ) =>
  NoThunks (SwapRaw era)

instance
  ( AlonzoEraScript era
  , NFData (TxBody era)
  , NFData (TxAuxData era)
  ) =>
  NFData (SwapRaw era)

deriving instance
  ( AlonzoEraScript era
  , Show (TxBody era)
  , Show (TxAuxData era)
  ) =>
  Show (SwapRaw era)

-- TODO WG I think you need to CHANGE this to `DecCBOR (Annotator (SwapRaw era))`. See TxAuxData

-- In fact, I think you need to make Swap abstract. I don't think you can have anything concrete at the TX level.
instance
  ( EraTxBody era
  , AlonzoEraScript era
  , DecCBOR (TxBody era)
  , DecCBOR (TxDats era)
  , Crypto (EraCrypto era)
  , DecCBOR (Redeemers era)
  , DecCBOR (WitVKey 'Witness (EraCrypto era))
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (SwapRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "SwapRaw"
        basicSwapRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (SwapRaw era)
      bodyFields 0 = field (\x tx -> tx {stxrTxBody = x}) From
      bodyFields 1 = field (\x tx -> tx {stxrWits = x}) From
      bodyFields 2 = field (\x tx -> tx {stxrDats = x}) From
      bodyFields 3 = field (\x tx -> tx {stxrRdmrs = x}) From
      bodyFields 4 = field (\x tx -> tx {stxrAux = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]

instance
  (AlonzoEraScript era, DecCBOR (Annotator (TxAuxData era)), EraTxBody era) =>
  DecCBOR (Annotator (SwapRaw era))
  where
  decCBOR =
    decode $
      SparseKeyed
        "Swap"
        (pure emptySwap)
        txWitnessField
        []
    where
      emptySwap = SwapRaw mkBasicTxBody mempty mempty mempty SNothing

      txWitnessField :: Word -> Field (Annotator (SwapRaw era))
      txWitnessField 0 =
        fieldAA
          (\x wits -> wits {stxrTxBody = x})
          From
      txWitnessField 1 =
        fieldAA
          (\x wits -> wits {stxrWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                ( allowTag setTag
                    >> mapTraverseableDecoderA (decodeNonEmptyList decCBOR) (Set.fromList . NE.toList)
                )
                (mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
          )
      txWitnessField 2 =
        fieldAA
          (\x wits -> wits {stxrDats = x})
          From
      txWitnessField 3 =
        fieldAA
          (\x wits -> wits {stxrRdmrs = x})
          From
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {stxrAux = x})
          ( D
              ( sequence . maybeToStrictMaybe
                  <$> decodeNullMaybe decCBOR
              )
          )
      txWitnessField n = field (\_ t -> t) (Invalid n)
      {-# INLINE txWitnessField #-}

deriving instance
  ( AlonzoEraScript era
  , NoThunks (TxBody era)
  , NoThunks (TxAuxData era)
  ) =>
  NoThunks (Swap era)

deriving instance
  ( AlonzoEraScript era
  , Eq (TxBody era)
  , Eq (TxAuxData era)
  ) =>
  Eq (Swap era)

deriving instance
  ( AlonzoEraScript era
  , NFData (TxBody era)
  , NFData (TxAuxData era)
  ) =>
  NFData (Swap era)

deriving instance
  ( AlonzoEraScript era
  , Show (TxBody era)
  , Show (TxAuxData era)
  ) =>
  Show (Swap era)

type instance MemoHashIndex SwapRaw = EraIndependentSwap

-- instance c ~ EraCrypto era => HashAnnotated (Swap era) EraIndependentSwap c where
--   hashAnnotated = getMemoSafeHash

-- instance
--   ( EraTxBody era
--   , AlonzoEraScript era
--   , Crypto (EraCrypto era)
--   , DecCBOR (WitVKey 'Witness (EraCrypto era))
--   , DecCBOR (TxBody era)
--   , DecCBOR (TxDats era)
--   , DecCBOR (Redeemers era)
--   , DecCBOR (TxAuxData era)
--   ) =>
--   DecCBOR (Annotator (SwapRaw era))
--   where
--   decCBOR = pure <$> decCBOR

deriving via
  (Mem SwapRaw era)
  instance
    ( DecCBOR (Annotator (TxAuxData era))
    , Era era
    , EraTxBody era
    , AlonzoEraScript era
    ) =>
    DecCBOR (Annotator (Swap era))

mkSwap :: (BabelEraTxBody era, AlonzoEraScript era, EncCBOR (TxAuxData era)) => Swap era
mkSwap = mkMemoized basicSwapRaw

basicSwapRaw :: (EraTxBody era, AlonzoEraScript era) => SwapRaw era
basicSwapRaw =
  SwapRaw
    mkBasicTxBody
    mempty
    mempty
    mempty
    SNothing

-- data SwapRaw era = SwapRaw
--   { stxTxBody :: TxBody era
--   , stxWits :: !(Set (WitVKey 'Witness (EraCrypto era)))
--   , stxDats :: !(TxDats era)
--   , stxRdmrs :: !(Redeemers era)
--   , stxAux :: !(StrictMaybe (TxAuxData era))
--   }
--   deriving (Generic, Typeable)

pattern BabelTxSwaps ::
  forall era.
  AlonzoEraScript era =>
  Strict.Map (TxId (EraCrypto era)) (Swap era) ->
  BabelTxSwaps era
pattern BabelTxSwaps {stxSwaps} <-
  (getMemoRawType -> BabelTxSwapsRaw stxSwaps)
  where
    BabelTxSwaps stxSwaps' =
      mkMemoized $ BabelTxSwapsRaw stxSwaps'

{-# COMPLETE BabelTxSwaps #-}

pattern Swap ::
  (AlonzoEraScript era, BabelEraTxBody era, EncCBOR (TxAuxData era)) =>
  TxBody era ->
  Set (WitVKey 'Witness (EraCrypto era)) ->
  TxDats era ->
  Redeemers era ->
  StrictMaybe (TxAuxData era) ->
  Swap era
pattern Swap
  { stxBody
  , stxWits
  , stxDats
  , stxRdmrs
  , stxAux
  } <-
  ( getMemoRawType ->
      SwapRaw
        { stxrTxBody = stxBody
        , stxrWits = stxWits
        , stxrDats = stxDats
        , stxrRdmrs = stxRdmrs
        , stxrAux = stxAux
        }
    )
  where
    Swap
      bodyX
      witsX
      datsX
      rdmrsX
      txAuxX =
        mkMemoized $
          SwapRaw
            bodyX
            witsX
            datsX
            rdmrsX
            txAuxX

{-# COMPLETE Swap #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeSwapRaw ::
  (BabelEraTxBody era, AlonzoEraScript era, EncCBOR (TxAuxData era)) =>
  SwapRaw era ->
  Encode ('Closed 'Sparse) (SwapRaw era)
encodeSwapRaw SwapRaw {..} =
  Keyed
    SwapRaw
    !> Key 0 (To stxrTxBody)
    !> Key 1 (To stxrWits)
    !> Key 2 (To stxrDats)
    !> Key 3 (To stxrRdmrs)
    !> encodeKeyedStrictMaybe 4 stxrAux

instance
  ( AlonzoEraScript era
  , BabelEraTxBody era
  , EncCBOR (TxAuxData era)
  ) =>
  EncCBOR (SwapRaw era)
  where
  encCBOR = encode . encodeSwapRaw

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (Swap era)
