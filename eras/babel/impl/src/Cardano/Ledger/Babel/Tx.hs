{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.Ledger.Babel.Tx where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Allegra.Core (Era (..), PParams)
import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx (isValidTxL),
  AlonzoTx (AlonzoTx),
  IsValid (..),
  alonzoEqTxRaw,
  totExUnits,
 )
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.TxAuxData ()
import Cardano.Ledger.Babel.TxBody (
  BabelEraTxBody (..),
 )
import Cardano.Ledger.Babel.TxWits ()
import Cardano.Ledger.BaseTypes (
  BoundedRational (unboundRational),
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  Encoding,
  ToCBOR,
  encodeListLen,
  encodeNullMaybe,
  serialize,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<*!))
import Cardano.Ledger.Binary.Decoding (decodeNullMaybe)
import Cardano.Ledger.Binary.Plain (ToCBOR (..))
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core (AlonzoEraTxWits, ConwayEraPParams, ppPricesL)
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Core (
  EraScript,
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxWits (..),
  Script,
  ScriptHash,
  Tx,
  bodyTxL,
  eraProtVerLow,
  hashScript,
  isNativeScript,
  ppMinFeeAL,
  toEraCBOR,
  upgradeTxAuxData,
  upgradeTxBody,
  upgradeTxWits,
  witsTxL,
 )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Plutus.ExUnits (txscriptfee)
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.TxIn (TxId)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldl', toList)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Maybe.Strict (maybeToStrictMaybe)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq, forceToStrict)
import GHC.Generics (Generic)
import Lens.Micro hiding (set)
import NoThunks.Class (NoThunks)

data BabelTx era = BabelTx
  { btxBody :: !(TxBody era)
  , btxWits :: !(TxWits era)
  , btxIsValid :: !IsValid
  , btxAuxiliaryData :: !(StrictMaybe (TxAuxData era))
  , -- , -- NEW
    --   btxSwaps :: !(StrictMaybe (Seq (TxId (EraCrypto era))))
    -- NEW
    btxSwaps :: !(StrictMaybe (StrictSeq (Tx era)))
  }
  deriving (Generic)

instance
  (Tx era ~ BabelTx era, BabelEraTx era) =>
  EqRaw (BabelTx era)
  where
  eqRaw = babelEqTxRaw

class
  ( EraTx era
  , AlonzoEraTx era
  , AlonzoEraTxWits era
  , BabelEraTxBody era
  ) =>
  BabelEraTx era
  where
  subTxTxL :: Lens' (Tx era) (StrictMaybe (StrictSeq (Tx era)))

-- batchValidTxL :: Lens' (Tx era) IsValid

instance Crypto c => BabelEraTx (BabelEra c) where
  subTxTxL = subTxBodiesBabelTxL
  {-# INLINE subTxTxL #-}

-- batchValidTxL = batchValidBabelTxL
-- {-# INLINE batchValidTxL #-}

mkBasicBabelTx ::
  Monoid (TxWits era) => TxBody era -> BabelTx era
mkBasicBabelTx txBody = BabelTx txBody mempty (IsValid True) SNothing mempty -- (IsValid True)

subTxBodiesBabelTxL ::
  Lens' (BabelTx era) (StrictMaybe (StrictSeq (Tx era)))
subTxBodiesBabelTxL = lens btxSwaps (\tx stx -> tx {btxSwaps = stx})
{-# INLINEABLE subTxBodiesBabelTxL #-}

-- | `TxBody` setter and getter for `BabelTx`.
bodyBabelTxL :: Lens' (BabelTx era) (TxBody era)
bodyBabelTxL = lens btxBody (\tx txBody -> tx {btxBody = txBody})
{-# INLINEABLE bodyBabelTxL #-}

-- | `TxWits` setter and getter for `BabelTx`.
witsBabelTxL :: Lens' (BabelTx era) (TxWits era)
witsBabelTxL = lens btxWits (\tx txWits -> tx {btxWits = txWits})
{-# INLINEABLE witsBabelTxL #-}

-- | `TxAuxData` setter and getter for `BabelTx`.
auxDataBabelTxL :: Lens' (BabelTx era) (StrictMaybe (TxAuxData era))
auxDataBabelTxL = lens btxAuxiliaryData (\tx txTxAuxData -> tx {btxAuxiliaryData = txTxAuxData})
{-# INLINEABLE auxDataBabelTxL #-}

toCBORForSizeComputation ::
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (Tx era)
  ) =>
  BabelTx era ->
  Encoding
toCBORForSizeComputation BabelTx {btxBody, btxWits, btxAuxiliaryData, btxSwaps} =
  encodeListLen 4
    <> encCBOR btxBody
    <> encCBOR btxWits
    <> encodeNullMaybe encCBOR (strictMaybeToMaybe btxAuxiliaryData)
    <> encCBOR btxSwaps

-- | txsize computes the length of the serialised bytes
sizeBabelTxF ::
  forall era.
  EraTx era =>
  SimpleGetter (BabelTx era) Integer
sizeBabelTxF =
  to $
    fromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeBabelTxF #-}

isValidBabelTxL :: Lens' (BabelTx era) IsValid
isValidBabelTxL = lens btxIsValid (\tx valid -> tx {btxIsValid = valid})
{-# INLINEABLE isValidBabelTxL #-}

deriving instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  , Eq (Tx era)
  ) =>
  Eq (BabelTx era)

deriving instance
  ( Era era
  , Show (TxBody era)
  , Show (TxAuxData era)
  , Show (Script era)
  , Show (TxWits era)
  , Show (Tx era)
  ) =>
  Show (BabelTx era)

instance
  ( Era era
  , NoThunks (TxWits era)
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (Tx era)
  ) =>
  NoThunks (BabelTx era)

instance
  ( Era era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , NFData (TxBody era)
  , NFData (Tx era)
  ) =>
  NFData (BabelTx era)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Construct an annotated Babel style transaction.
babelSegwitTx ::
  BabelEraTx era =>
  (Seq (TxId (EraCrypto era)) -> Seq (Tx era)) ->
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  IsValid ->
  Maybe (Annotator (TxAuxData era)) ->
  -- TODO WG these might be wrong
  Maybe (Seq (TxId (EraCrypto era))) ->
  Annotator (Tx era)
babelSegwitTx mkMap txBodyAnn txWitsAnn btxIsValid auxDataAnn swapsAnn = Annotator $ \bytes ->
  let txBody = runAnnotator txBodyAnn bytes
      txWits = runAnnotator txWitsAnn bytes
      txAuxData = maybeToStrictMaybe (flip runAnnotator bytes <$> auxDataAnn)
   in mkBasicTx txBody
        & witsTxL
        .~ txWits
        & auxDataTxL
        .~ txAuxData
        & isValidTxL
        .~ btxIsValid
        & subTxTxL
        .~ maybeToStrictMaybe (fmap (forceToStrict . mkMap) swapsAnn)

--------------------------------------------------------------------------------
-- Mempool Serialisation
--
-- We do not store the Tx bytes for the following reasons:
-- - A Tx serialised in this way never forms part of any hashed structure, hence
--   we do not worry about the serialisation changing and thus seeing a new
--   hash.
-- - The three principal components of this Tx already store their own bytes;
--   here we simply concatenate them. The final component, `IsValid`, is
--   just a flag and very cheap to serialise.
--------------------------------------------------------------------------------

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node.
--
-- Note that this serialisation is neither the serialisation used on-chain
-- (where Txs are deconstructed using segwit), nor the serialisation used for
-- computing the transaction size (which omits the `IsValid` field for
-- compatibility with Mary - see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (Tx era)
  ) =>
  BabelTx era ->
  Encoding
toCBORForMempoolSubmission
  BabelTx
    { btxBody
    , btxWits
    , btxAuxiliaryData
    , btxIsValid
    , btxSwaps
    } =
    encode $
      Rec BabelTx
        !> To btxBody
        !> To btxWits
        !> To btxIsValid
        !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) btxAuxiliaryData
        !> To btxSwaps

instance
  ( Era era
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  , EncCBOR (Tx era)
  ) =>
  EncCBOR (BabelTx era)
  where
  encCBOR = toCBORForMempoolSubmission

instance
  ( Era era
  , EncCBOR (TxBody era)
  , EncCBOR (TxAuxData era)
  , EncCBOR (TxWits era)
  , EncCBOR (Tx era)
  ) =>
  ToCBOR (BabelTx era)
  where
  toCBOR = toEraCBOR @era

instance
  EraTx era =>
  DecCBOR (Annotator (BabelTx era))
  where
  decCBOR =
    decode $
      Ann (RecD BabelTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )
        <*! D (traverse sequence <$> decCBOR)
  -- <*! Ann From
  {-# INLINE decCBOR #-}

-- =======================================================================
-- Some generic functions that compute over Tx. We try to be abstract over
-- things that might differ from Era to Era like
--    1) TxOut will have additional fields
--    2) Scripts might appear in places other than the witness set. So
--       we need such a 'witness' we pass it as a parameter and each call site
--       can use a different method to compute it in the current Era.

-- | Compute if an Addr has the hash of a TwoPhaseScript, we can tell
--   what kind of Script from the Hash, by looking it up in the Map
isTwoPhaseScriptAddressFromMap ::
  forall era.
  EraScript era =>
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  Addr (EraCrypto era) ->
  Bool
isTwoPhaseScriptAddressFromMap hashScriptMap addr =
  case Shelley.getScriptHash @(EraCrypto era) addr of
    Nothing -> False
    Just hash -> any ok hashScriptMap
      where
        ok script = hashScript @era script == hash && not (isNativeScript @era script)

babelEqTxRaw ::
  BabelEraTx era =>
  Tx era ->
  Tx era ->
  Bool
babelEqTxRaw tx1 tx2 =
  alonzoEqTxRaw tx1 tx2
    && ( (tx1 ^. subTxTxL) == (tx2 ^. subTxTxL)
       )

instance
  Crypto c =>
  Core.EraTx (BabelEra c)
  where
  type Tx (BabelEra c) = BabelTx (BabelEra c)
  type TxUpgradeError (BabelEra c) = Core.TxBodyUpgradeError (BabelEra c)

  mkBasicTx = mkBasicBabelTx

  bodyTxL = bodyBabelTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsBabelTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataBabelTxL
  {-# INLINE auxDataTxL #-}

  -- requiredTxsTxL = lens (const mempty) const
  -- {-# INLINE requiredTxsTxL #-}

  sizeTxF = sizeBabelTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx = getBabelMinFeeTx

  upgradeTx (AlonzoTx b w valid aux) =
    BabelTx
      <$> upgradeTxBody b
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)
      <*> pure mempty

-- <*> pure valid

mkBabelEraTx ::
  BabelEraTx era =>
  TxBody era ->
  TxWits era ->
  IsValid ->
  StrictMaybe (TxAuxData era) ->
  StrictMaybe (StrictSeq (Tx era)) ->
  Tx era
mkBabelEraTx body wits isValid auxData swaps =
  mkBasicTx body
    & witsTxL
    .~ wits
    & isValidTxL
    .~ isValid
    & auxDataTxL
    .~ auxData
    & subTxTxL
    .~ swaps

getBabelMinFeeTx ::
  ( ConwayEraPParams era
  , BabelEraTx era
  ) =>
  PParams era ->
  Tx era ->
  Int ->
  Coin
getBabelMinFeeTx pp tx refScriptsSize =
  ( tx
      ^. sizeTxF
      <×> pp
      ^. ppMinFeeAL
      <+> txscriptfee (pp ^. ppPricesL) allExunits
  )
    <+> refScriptsFee
  where
    allExunits =
      foldl' (<>) mempty $
        fmap
          totExUnits
          (tx : (concat . maybeToList . strictMaybeToMaybe) (fmap toList (tx ^. subTxTxL)))
    refScriptCostPerByte = unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL)
    refScriptsFee = Coin (floor (fromIntegral @Int @Rational refScriptsSize * refScriptCostPerByte))

instance Crypto c => AlonzoEraTx (BabelEra c) where
  isValidTxL = isValidBabelTxL
  {-# INLINE isValidTxL #-}