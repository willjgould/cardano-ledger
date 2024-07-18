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

import Cardano.Ledger.NonIntegral
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
import GHC.Generics (Generic)
import LeaderElection (
  BoundedRatio (..),
  PositiveUnitInterval (..),
  assertBoundedNatural,
  checkLeaderNatValue,
  mkActiveSlotCoeff,
 )
import Statistics.Types (Estimate (..))
import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.Printf (printf)

deriving instance Generic (CompareResult a)
instance NFData a => NFData (CompareResult a)

main :: IO ()
main = do
  defaultMainWith config benchmarks
  (Right (_, _, reports)) <- readJSONReports reportJsonName
  generateTypstReport reports

-- We can use a template to change the html output. Might be easier than using typst.
config :: Config
config = defaultConfig {jsonFile = Just reportJsonName, template = "./bench/templates/my_template.tpl"}

reportJsonName :: String
reportJsonName = "report.json"

benchmarks :: [Benchmark]
benchmarks =
  [ benchGroup "exp'" exp'
  , benchGroup "ln'" ln'
  , bgroup
      "exponentiation"
      [ bench "small_int" $ whnf ((2 :: Double) ***) 3
      , bench "small_frac" $ whnf ((1.5 :: Double) ***) 2.5
      , bench "medium" $ whnf ((5 :: Double) ***) 10
      , bench "large" $ whnf ((10 :: Double) ***) 20000
      ]
  , bgroup
      "findE"
      [ bench "small" $ whnf (findE (exp (1 :: Double))) 2
      , bench "small_frac" $ whnf (findE (exp (1 :: Double))) 2.5
      , bench "medium" $ whnf (findE (exp (1 :: Double))) 100
      , bench "large" $ whnf (findE (exp (1 :: Double))) 10000
      ]
  , bgroup
      "taylorExpCmp"
      [ bench "small" $ nf (taylorExpCmp (1 :: Double) 1) 0.1
      , bench "medium" $ nf (taylorExpCmp (2 :: Double) 2) 1.5
      , bench "large" $ nf (taylorExpCmp (10 :: Double) 100) 5
      ]
  , bgroup
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
  where
    benchGroup :: String -> (Double -> Double) -> Benchmark
    benchGroup name f =
      bgroup
        name
        [ bench "small" $ whnf f 0.1
        , bench "one" $ whnf f 1
        , bench "medium" $ whnf f 1.5
        , bench "medium_large" $ whnf f 25
        , bench "large" $ whnf f 100.0
        ]

-- Example handcranked (not currently useful) typst
generateTypstReport :: [Report] -> IO ()
generateTypstReport reports = do
  let typstContent =
        unlines $
          [ "#let project(title: \"\", body) = {"
          , "  set document(title: title)"
          , "  align(center)[#block(text(2em, title))]"
          , "  body"
          , "}"
          , ""
          , "#show: project.with("
          , "  title: \"NonIntegral Benchmark Results Summary\","
          , ")"
          , ""
          ]
            ++ fmap reportToTypst reports

  writeFile "report.typ" typstContent

reportToTypst :: Report -> String
reportToTypst Report {..} =
  unlines
    [ "== " ++ reportName
    , ""
    , "=== Analysis"
    , ""
    , "#table("
    , "  columns: (auto, auto, auto),"
    , "  [Metric], [Value], [Unit],"
    , "  [Mean], [" ++ showDouble (estPoint $ anMean reportAnalysis) ++ "], [seconds],"
    , "  [Std Dev], [" ++ showDouble (estPoint $ anStdDev reportAnalysis) ++ "], [seconds],"
    , "  [Sample Size], [" ++ show (V.length reportMeasured) ++ "], [measurements],"
    , ")"
    , ""
    , "=== Outliers"
    , ""
    , "#table("
    , "  columns: (auto, auto),"
    , "  [Category], [Count],"
    , "  [Low Mild], [" ++ show (lowMild reportOutliers) ++ "],"
    , "  [Low Severe], [" ++ show (lowSevere reportOutliers) ++ "],"
    , "  [High Mild], [" ++ show (highMild reportOutliers) ++ "],"
    , "  [High Severe], [" ++ show (highSevere reportOutliers) ++ "],"
    , ")"
    , ""
    , "Total samples seen: " ++ show (samplesSeen reportOutliers)
    , ""
    ]

showDouble :: Double -> String
showDouble = printf "%.6f"
