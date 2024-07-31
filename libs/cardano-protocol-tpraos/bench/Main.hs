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

-- | This is a strict version of &&.
(&&!) :: Bool -> Bool -> Bool
True &&! True = True
True &&! False = False
False &&! True = False
False &&! False = False

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "checkLeaderNatValue"
      [ bench
          "0.01%"
          $ nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 10000) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 10000) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      , bench "0.1%" $
          nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 1000) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 1000) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      , bench "1%" $
          nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 100) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 100) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      , bench "2%" $
          nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 50) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 50) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      , bench "5%" $
          nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 20) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 20) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      , bench "10%" $
          nf
            ( \x ->
                checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 10) x
                  &&! checkLeaderNatValue (assertBoundedNatural 10 1) (1 % 10) x
            )
            (mkActiveSlotCoeff $ PositiveUnitInterval $ BoundedRatio $ 1 % 10)
      ]
  ]
