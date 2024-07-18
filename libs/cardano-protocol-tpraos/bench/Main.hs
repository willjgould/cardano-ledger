{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Cardano.Ledger.BaseTypes (
  BoundedRatio (..),
  PositiveUnitInterval (PositiveUnitInterval),
  mkActiveSlotCoeff,
 )
import Cardano.Protocol.TPraos.API (getLeaderSchedule)
import Cardano.Protocol.TPraos.BHeader (assertBoundedNatural, checkLeaderNatValue)
import Control.DeepSeq (NFData)
import Criterion.IO (readJSONReports)
import Criterion.Main
import Criterion.Types (
  Config (..),
  Outliers (..),
  Report (..),
  SampleAnalysis (anMean, anStdDev),
 )
import Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Statistics.Types (Estimate (..))
import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.Printf (printf)

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "checkLeaderNatValue"
      [ bench "ones" $
          nf
            (checkLeaderNatValue (assertBoundedNatural 1 1) (1 % 1))
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 1)
      , bench "lower-range" $
          nf
            (checkLeaderNatValue (assertBoundedNatural 1000000 1) (1 % 1000000))
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 1000000)
      , bench "mid-range" $
          nf
            (checkLeaderNatValue (assertBoundedNatural 1000 500) (1 % 2))
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 2)
      , bench "upper-range" $
          nf
            (checkLeaderNatValue (assertBoundedNatural 1000000 999999) (999999 % 1000000))
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 999999 % 1000000)
      ]
  ]