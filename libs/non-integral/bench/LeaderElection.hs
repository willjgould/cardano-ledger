{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LeaderElection where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, decodeRationalWithTag)
import Cardano.Ledger.Binary.Decoding (DecCBOR (..), DecoderError (..), cborError)
import Cardano.Ledger.Binary.Encoding (EncCBOR (..), encodeRatioWithTag)
import Cardano.Ledger.NonIntegral (CompareResult (..), ln', taylorExpCmp)
import Control.DeepSeq (NFData)
import Control.Monad (when, (<=<))
import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import Data.Aeson.Types (FromJSON (..))
import Data.Data (Typeable)
import qualified Data.Fixed as FP
import Data.Ratio (Ratio, denominator, (%))
import Data.Scientific (
  Scientific (base10Exponent, coefficient),
  fromRationalRepetendLimited,
  normalize,
 )
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Real (numerator)
import NoThunks.Class (NoThunks)

checkLeaderNatValue ::
  -- | Certified nat value
  BoundedNatural ->
  -- | Stake proportion
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderNatValue bn σ f =
  if activeSlotVal f == maxBound
    then -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
      True
    else case taylorExpCmp 3 recip_q x of
      ABOVE _ _ -> False
      BELOW _ _ -> True
      MaxReached _ -> False
  where
    c, recip_q, x :: FixedPoint
    c = activeSlotLog f
    recip_q = fromRational (toInteger certNatMax % toInteger (certNatMax - certNat))
    x = -fromRational σ * c
    certNatMax = bvMaxValue bn
    certNat = bvValue bn

activeSlotVal :: ActiveSlotCoeff -> PositiveUnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = fromIntegral (unActiveSlotLog f) / fpPrecision

-- | Assert that a natural is bounded by a certain value. Throws an error when
-- this is not the case.
assertBoundedNatural ::
  -- | Maximum bound
  Natural ->
  -- | Value
  Natural ->
  BoundedNatural
assertBoundedNatural maxVal val =
  if val <= maxVal
    then UnsafeBoundedNatural maxVal val
    else error $ show val <> " is greater than max value " <> show maxVal

mkActiveSlotCoeff :: PositiveUnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff
    { unActiveSlotVal = v
    , unActiveSlotLog =
        if v == maxBound
          then -- If the active slot coefficient is equal to one,
          -- then nearly every stake pool can produce a block every slot.
          -- In this degenerate case, where ln (1-f) is not defined,
          -- we set the unActiveSlotLog to zero.
            0
          else
            floor
              (fpPrecision * ln' ((1 :: FixedPoint) - fromRational (unboundRational v)))
    }

-- | Natural value with some additional bound. It must always be the base that
-- 'bvValue <= bvMaxValue'. The creator is responsible for checking this value.
data BoundedNatural = UnsafeBoundedNatural
  { bvMaxValue :: Natural
  , bvValue :: Natural
  }

data ActiveSlotCoeff = ActiveSlotCoeff
  { unActiveSlotVal :: !PositiveUnitInterval
  , unActiveSlotLog :: !Integer -- TODO mgudemann make this FixedPoint,
  -- currently a problem because of
  -- NoThunks instance for FixedPoint
  }
  deriving (Eq, Ord, Show, Generic)

-- | Type to represent a value in the unit interval (0; 1]
newtype PositiveUnitInterval
  = PositiveUnitInterval (BoundedRatio PositiveUnitInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show
    , Bounded
    , BoundedRational
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
    )

instance Bounded (BoundedRatio PositiveUnitInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (1 % 1)

-- | The smallest decimal value that can roundtrip JSON
positiveIntervalEpsilon :: BoundedRatio b Word64
positiveIntervalEpsilon = BoundedRatio (1 % 10 ^ (maxDecimalsWord64 :: Int))

-- | Type clases that allows conversion between `Rational` and some form of bounded
-- rational type. Bounds can be restricted by both the `Bounded` type class and underlyng
-- representation.
--
-- > maybe True (\br -> minBound <= br && br <= maxBound) . boundRational
--
-- Roundtrip properties must hold:
--
-- > \r -> maybe True ((r ==) . unboundRational) (boundRational r)
-- > \br -> Just br == boundRational (unboundRational br)
class Bounded r => BoundedRational r where
  -- | Returns `Nothing` when supplied value is not within bounds or when precision is
  -- too high to be represented by the underlying type
  --
  -- ===__Example__
  --
  -- >>> :set -XTypeApplications
  -- >>> import Data.Ratio
  -- >>> boundRational @UnitInterval $ 2 % 3
  -- Just (2 % 3)
  -- >>> boundRational @UnitInterval (-0.5)
  -- Nothing
  -- >>> boundRational @UnitInterval (1.5)
  -- Nothing
  -- >>> boundRational @UnitInterval 0
  -- Just (0 % 1)
  -- >>> boundRational @PositiveUnitInterval 0
  -- Nothing
  boundRational :: Rational -> Maybe r

  -- | Promote bounded rational type into the unbounded `Rational`.
  unboundRational :: r -> Rational

instance
  (Bounded (BoundedRatio b a), Bounded a, Integral a) =>
  BoundedRational (BoundedRatio b a)
  where
  boundRational = fromRationalBoundedRatio
  unboundRational = toRationalBoundedRatio

-- | This is an internal type for representing rational numbers that are bounded on some
-- interval that is controlled by phantom type variable @b@ as well as by
-- the bounds of underlying type @a@.
newtype BoundedRatio b a = BoundedRatio (Ratio a)
  deriving (Eq, Generic)
  deriving newtype (Show, NoThunks, NFData)

-- Deriving Ord instance can lead to integer overflow. We must go through Rational.
instance Integral a => Ord (BoundedRatio b a) where
  compare (BoundedRatio a) (BoundedRatio b) = compare (promoteRatio a) (promoteRatio b)

promoteRatio :: Integral a => Ratio a -> Rational
promoteRatio r = toInteger (numerator r) % toInteger (denominator r)

data E34

instance FP.HasResolution E34 where
  resolution _ = (10 :: Integer) ^ (34 :: Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10 :: FixedPoint) ^ (34 :: Integer)

toRationalBoundedRatio :: Integral a => BoundedRatio b a -> Rational
toRationalBoundedRatio (BoundedRatio r) = promoteRatio r

fromRationalBoundedRatio ::
  forall b a.
  (Bounded (BoundedRatio b a), Bounded a, Integral a) =>
  Rational ->
  Maybe (BoundedRatio b a)
fromRationalBoundedRatio r
  | n < minVal || d < minVal || n > maxVal || d > maxVal = Nothing -- protect against overflow
  | otherwise = fromRatioBoundedRatio $ fromInteger n % fromInteger d
  where
    minVal = toInteger (minBound :: a)
    maxVal = toInteger (maxBound :: a)
    n = numerator r
    d = denominator r

-- | Convert to `BoundedRatio`, while checking the bounds. This function doesn't guard
-- against overflow, therefore use `fromRationalBoundedRatio . promoteRatio` instead
-- when in doubt.
fromRatioBoundedRatio ::
  forall b a.
  (Bounded a, Bounded (BoundedRatio b a), Integral a) =>
  Ratio a ->
  Maybe (BoundedRatio b a)
fromRatioBoundedRatio ratio
  | r < unboundRational lowerBound
      || r > unboundRational upperBound =
      Nothing -- ensure valid range
  | otherwise = Just $ BoundedRatio ratio
  where
    r = promoteRatio ratio
    lowerBound = minBound :: BoundedRatio b a
    upperBound = maxBound :: BoundedRatio b a

instance (EncCBOR a, Integral a, Bounded a, Typeable b) => EncCBOR (BoundedRatio b a) where
  encCBOR (BoundedRatio u) = encodeRatioWithTag encCBOR u

instance
  (DecCBOR a, Bounded (BoundedRatio b a), Bounded a, Integral a, Typeable b, Show a) =>
  DecCBOR (BoundedRatio b a)
  where
  decCBOR = do
    r <- decodeRationalWithTag
    case boundRational r of
      Nothing ->
        cborError $ DecoderErrorCustom "BoundedRatio" (Text.pack $ show r)
      Just u -> pure u

instance Bounded (BoundedRatio b Word64) => ToJSON (BoundedRatio b Word64) where
  toJSON :: BoundedRatio b Word64 -> Value
  toJSON br = case fromRationalRepetendLimited maxDecimalsWord64 r of
    Right (s, Nothing) -> toJSON s
    _ -> toJSON r
    where
      r = unboundRational br

instance Bounded (BoundedRatio b Word64) => FromJSON (BoundedRatio b Word64) where
  parseJSON = \case
    rational@(Object _) -> parseWith fromRationalEither rational
    sci -> parseWith fromScientificBoundedRatioWord64 sci
    where
      parseWith f = either fail pure . f <=< parseJSON

maxDecimalsWord64 :: Int
maxDecimalsWord64 = 19

fromScientificBoundedRatioWord64 ::
  Bounded (BoundedRatio b Word64) =>
  Scientific ->
  Either String (BoundedRatio b Word64)
fromScientificBoundedRatioWord64 (normalize -> sci)
  | coeff < 0 = failWith "negative" sci
  | exp10 <= 0 = do
      when (exp10 < -maxDecimalsWord64) $ failWith "too precise" sci
      fromRationalEither (coeff % (10 ^ negate exp10))
  | otherwise = do
      when (maxDecimalsWord64 < exp10) $ failWith "too big" sci
      fromRationalEither (coeff * 10 ^ exp10 % 1)
  where
    coeff = coefficient sci
    exp10 = base10Exponent sci

fromRationalEither ::
  Bounded (BoundedRatio b Word64) => Rational -> Either String (BoundedRatio b Word64)
fromRationalEither r = maybe (failWith "outside of bounds" r) Right $ boundRational r

failWith :: Show a => String -> a -> Either String b
failWith msg val = Left $ "Value is " <> msg <> ": " <> show val