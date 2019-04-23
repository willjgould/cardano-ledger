{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Cardano.Prelude

import           Cardano.BM.Data.LogItem        (LoggerName)
import           Cardano.Shell.Features.Logging (LoggingLayer (..), Trace,
                                                 createLoggingFeature)
import           Cardano.Shell.Lib              (runCardanoApplicationWithFeatures)
import           Cardano.Shell.Presets          (mainnetConfiguration)
import           Cardano.Shell.Types            (ApplicationEnvironment (..),
                                                 CardanoApplication (..),
                                                 initializeCardanoEnvironment)
import           Control.Concurrent             (threadDelay)
import           Features.Blockchain            (BlockchainLayer (..),
                                                 createBlockchainFeature)


main :: IO ()
main = do
  -- This is where the configuration and environment should come from; these
  -- values are currently thrown away in `createLoggingFeature`. The
  -- `createBlockchainFeature` only uses 'CardanoConfiguration' at the moment.
  let cardanoConfiguration = mainnetConfiguration
  cardanoEnvironment <- initializeCardanoEnvironment

  -- Features 'blockchainApp' will use.
  (loggingLayer, loggingFeature) <- createLoggingFeature
    cardanoEnvironment
    cardanoConfiguration

  let logTrace :: Trace IO Text
      logTrace = llBasicTrace loggingLayer

      appendName :: LoggerName -> Trace IO Text -> IO (Trace IO Text)
      appendName = llAppendName loggingLayer

  mainTrace <- appendName "cardano-ledger" logTrace

  (blockchainLayer, blockchainFeature) <- createBlockchainFeature mainTrace
    cardanoEnvironment
    cardanoConfiguration
    Production
    loggingLayer

  -- Run application.
  runCardanoApplicationWithFeatures
      Production
      [blockchainFeature, loggingFeature]
    . CardanoApplication
    $ blockchainApp mainTrace loggingLayer blockchainLayer


-- The overall application. These are various
-- loops that check the 'BlockchainLayer' to
-- get the status of a particular IO action
-- defined in 'init'
blockchainApp :: LoggingLayer -> BlockchainLayer -> IO ()
blockchainApp ll bcl = do
  mainTrace <- llAppendName ll "validate-mainnet" (llBasicTrace ll)

  -- Bulk chain validation
  bulkChainValidation mainTrace bcl ll


bulkChainValidation :: Trace IO Text -> BlockchainLayer -> LoggingLayer -> IO ()
bulkChainValidation logTrace bcl ll = do
  logNotice logTrace "Begin validating epoch files..."
  bulkChainValidationLoop logTrace
 where
  logNotice :: Trace IO Text -> Text -> IO ()
  logNotice = llLogNotice ll

  -- Checks status of bulk chain validation.
  bulkChainValidationLoop :: Trace IO Text -> IO ()
  bulkChainValidationLoop lgTrace = chainValidationStatus bcl >>= \case
    Nothing -> do
      threadDelay 1e6
      bulkChainValidationLoop lgTrace
    Just results -> case results of
      Left epochError -> do
        logNotice lgTrace "Epoch validation failed!"
        logNotice lgTrace $ show epochError
        exitFailure
      Right _ -> do
        logNotice lgTrace "Epoch validation successful!"
        logNotice lgTrace "Finished bulk chain validation."
