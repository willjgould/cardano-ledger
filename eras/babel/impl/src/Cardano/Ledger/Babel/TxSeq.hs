{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | TxSeq. This is effectively the block body, which consists of a sequence of
-- transactions with segregated witness and metadata information.
module Cardano.Ledger.Babel.TxSeq (
  BabelTxSeq (BabelTxSeq, txSeqTxns),
  TxSeq,
  hashTxSeq,
  hashBabelTxSeq,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..), alonzoSegwitTx)
import Cardano.Ledger.Babel.Era
import Cardano.Ledger.Babel.Tx
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBORGroup (..),
  encCBOR,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  encodedSizeExpr,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Core hiding (TxSeq, hashTxSeq)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.SafeHash (SafeToHash, originalBytes)
import Cardano.Ledger.Shelley.BlockChain (constructMetadata)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (AllowThunksIn (..), NoThunks)

-- =================================================

-- $TxSeq
--
-- * TxSeq
--
-- TxSeq provides an alternate way of formatting transactions in a block, in
-- order to support segregated witnessing.

data BabelTxSeq era = BabelTxSeqRaw
  { txSeqTxns :: !(StrictSeq (Tx era))
  , txSeqBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('AlonzoTxBody' era)@
  , txSeqWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWitness' era)@
  , txSeqMetadataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @Map Int ('AuxiliaryData')@. Missing indices have
  -- 'SNothing' for metadata
  , txSeqIsValidBytes :: BSL.ByteString
  -- ^ Bytes representing a set of integers. These are the indices of
  -- transactions with 'isValid' == False.
  , txSeqIsTopLevel :: BSL.ByteString -- Represents Seq Bool
  , txSeqSwaps :: BSL.ByteString -- Represents Seq (Strict.Map (TxId (EraCrypto era)) (TxSwaps era))
  , txSeqRequiredTxBodies :: BSL.ByteString -- Represents Seq (Strict.Map (TxId (EraCrypto era)) (TxBody era))
  }
  deriving (Generic)

instance Crypto c => EraSegWits (BabelEra c) where
  type TxSeq (BabelEra c) = BabelTxSeq (BabelEra c)
  fromTxSeq :: Crypto c => Core.TxSeq (BabelEra c) -> StrictSeq (Tx (BabelEra c))
  fromTxSeq = txSeqTxns
  toTxSeq = BabelTxSeq
  hashTxSeq = hashBabelTxSeq
  numSegComponents = 7

pattern BabelTxSeq ::
  forall era.
  ( BabelEraTx era
  , SafeToHash (TxSwaps era)
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  BabelTxSeq era
pattern BabelTxSeq xs <-
  BabelTxSeqRaw xs _ _ _ _ _ _ _
  where
    BabelTxSeq txns =
      let version = eraProtVerLow @era
          serializeFoldablePreEncoded x =
            serialize version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodeIndexed <$> strictMaybeToMaybe m
            where
              encodeIndexed metadata = encCBOR index <> encodePreEncoded metadata
       in BabelTxSeqRaw
            { txSeqTxns = txns
            , txSeqBodyBytes =
                serializeFoldablePreEncoded $ originalBytes . view bodyTxL <$> txns
            , txSeqWitsBytes =
                serializeFoldablePreEncoded $ originalBytes . view witsTxL <$> txns
            , txSeqMetadataBytes =
                serialize version . encodeFoldableMapEncoder metaChunk $
                  fmap originalBytes . view auxDataTxL <$> txns
            , txSeqIsValidBytes =
                serialize version $ encCBOR $ nonValidatingIndices txns
            , -- This should probably be like nonValidatingIndices because there's only one top level tx
              txSeqIsTopLevel = serialize version $ encCBOR $ view isTopLevelTxL <$> txns -- Also, is this right? What about `originalBytes`?
            , txSeqSwaps =
                serializeFoldablePreEncoded $ originalBytes . view subTxBodiesTxL <$> txns
            , txSeqRequiredTxBodies =
                serializeFoldablePreEncoded $ originalBytes . view requiredTxBodiesTxL <$> txns
            }

{-# COMPLETE BabelTxSeq #-}

type TxSeq era = BabelTxSeq era

{-# DEPRECATED TxSeq "Use `BabelTxSeq` instead" #-}

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes"
     , "txSeqWitsBytes"
     , "txSeqMetadataBytes"
     , "txSeqIsValidBytes"
     , "txSeqIsTopLevel"
     , "txSeqSwaps"
     , "txSeqRequiredTxBodies"
     ]
    (TxSeq era)
  instance
    (Typeable era, NoThunks (Tx era)) => NoThunks (TxSeq era)

deriving stock instance Show (Tx era) => Show (TxSeq era)

deriving stock instance Eq (Tx era) => Eq (TxSeq era)

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance Era era => EncCBORGroup (TxSeq era) where
  encCBORGroup (BabelTxSeqRaw _ bodyBytes witsBytes metadataBytes invalidBytes topLevel swaps requiredTxBodies) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes <> invalidBytes <> topLevel <> swaps <> requiredTxBodies
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 7
  listLenBound _ = 7

hashTxSeq ::
  forall era.
  Era era =>
  BabelTxSeq era ->
  Hash (EraCrypto era) EraIndependentBlockBody
hashTxSeq = hashBabelTxSeq
{-# DEPRECATED hashTxSeq "Use `hashBabelTxSeq` instead" #-}

-- | Hash a given block body
hashBabelTxSeq ::
  forall era.
  Era era =>
  BabelTxSeq era ->
  Hash (EraCrypto era) EraIndependentBlockBody
hashBabelTxSeq (BabelTxSeqRaw _ bodies ws md vs topLevel swaps requiredTxBodies) =
  coerce $
    hashStrict $
      BSL.toStrict $
        toLazyByteString $
          mconcat
            [ hashPart bodies
            , hashPart ws
            , hashPart md
            , hashPart vs
            , hashPart topLevel
            , hashPart swaps
            , hashPart requiredTxBodies
            ]
  where
    hashStrict :: ByteString -> Hash (EraCrypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = shortByteString . Hash.hashToBytesShort . hashStrict . BSL.toStrict

instance BabelEraTx era => DecCBOR (Annotator (TxSeq era)) where
  decCBOR = do
    (bodies, bodiesAnn) <- withSlice decCBOR
    (ws, witsAnn) <- withSlice decCBOR
    (topLevel, topLevelAnn) <- withSlice decCBOR
    (swaps, swapsAnn) <- withSlice decCBOR
    (requiredTxBodies, requiredTxBodiesAnn) <- withSlice decCBOR
    let b = length bodies
        inRange x = (0 <= x) && (x <= (b - 1))
        w = length ws
    (auxData, auxDataAnn) <- withSlice $
      do
        m <- decCBOR
        unless
          (all inRange (Map.keysSet m))
          ( fail
              ( "Some Auxiliarydata index is not in the range: 0 .. "
                  ++ show (b - 1)
              )
          )
        pure (constructMetadata b m)
    (isValIdxs, isValAnn) <- withSlice decCBOR
    let vs = alignedValidFlags b isValIdxs
    unless
      (b == w)
      ( fail $
          "different number of transaction bodies ("
            <> show b
            <> ") and witness sets ("
            <> show w
            <> ")"
      )
    unless
      (all inRange isValIdxs)
      ( fail
          ( "Some IsValid index is not in the range: 0 .. "
              ++ show (b - 1)
              ++ ", "
              ++ show isValIdxs
          )
      )

    let txns =
          sequenceA $
            StrictSeq.forceToStrict $
              zipWith7 babelSegwitTx bodies ws vs auxData topLevel swaps requiredTxBodies
    pure $
      BabelTxSeqRaw
        <$> txns
        <*> bodiesAnn
        <*> witsAnn
        <*> auxDataAnn
        <*> isValAnn
        <*> topLevelAnn
        <*> swapsAnn
        <*> requiredTxBodiesAnn

--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------

zipWith5 :: (a -> b -> c -> d -> e -> f) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e -> Seq f
zipWith5 f xs ys zs ws = Seq.zipWith ($) (Seq.zipWith4 f xs ys zs ws)

zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g) -> Seq a -> Seq b -> Seq c -> Seq d -> Seq e -> Seq f -> Seq g
zipWith6 f xs ys zs ws vs = Seq.zipWith ($) (zipWith5 f xs ys zs ws vs)

zipWith7 ::
  (a -> b -> c -> d -> e -> f -> g -> h) ->
  Seq a ->
  Seq b ->
  Seq c ->
  Seq d ->
  Seq e ->
  Seq f ->
  Seq g ->
  Seq h
zipWith7 f xs ys zs ws vs ts = Seq.zipWith ($) (zipWith6 f xs ys zs ws vs ts)

-- | Given a sequence of transactions, return the indices of those which do not
-- validate. We store the indices of the non-validating transactions because we
-- expect this to be a much smaller set than the validating transactions.
nonValidatingIndices :: AlonzoEraTx era => StrictSeq (Tx era) -> [Int]
nonValidatingIndices (StrictSeq.fromStrict -> xs) =
  Seq.foldrWithIndex
    ( \idx tx acc ->
        if tx ^. isValidTxL == IsValid False
          then idx : acc
          else acc
    )
    []
    xs

-- | Given the number of transactions, and the set of indices for which these
-- transactions do not validate, create an aligned sequence of `IsValid`
-- flags.
--
-- This function operates much as the inverse of 'nonValidatingIndices'.
alignedValidFlags :: Int -> [Int] -> Seq.Seq IsValid
alignedValidFlags = alignedValidFlags' (-1)
  where
    alignedValidFlags' _ n [] = Seq.replicate n $ IsValid True
    alignedValidFlags' prev n (x : xs) =
      Seq.replicate (x - prev - 1) (IsValid True)
        Seq.>< IsValid False
        Seq.<| alignedValidFlags' x (n - (x - prev)) xs