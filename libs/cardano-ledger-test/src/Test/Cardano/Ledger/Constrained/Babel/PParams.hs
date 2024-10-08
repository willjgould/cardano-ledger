{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.Babel.PParams where

import Cardano.Ledger.Babel.PParams
import Cardano.Ledger.BaseTypes

import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Babel.Instances ()
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)

pparamsSpec :: IsConwayUniv fn => Specification fn (PParams (BabelEra StandardCrypto))
pparamsSpec =
  constrained $ \pp ->
    match pp $ \cpp ->
      match cpp $
        \_cppMinFeeA
         _cppMinFeeB
         cppMaxBBSize
         cppMaxTxSize
         cppMaxBHSize
         _cppKeyDeposit
         cppPoolDeposit
         cppEMax
         _cppNOpt
         _cppA0
         _cppRho
         _cppTau
         _cppProtocolVersion
         _cppMinPoolCost
         _cppCoinsPerUTxOByte
         _cppCostModels
         _cppPrices
         _cppMaxTxExUnits
         _cppMaxBlockExUnits
         cppMaxValSize
         cppCollateralPercentage
         _cppMaxCollateralInputs
         _cppPoolVotingThresholds
         _cppDRepVotingThresholds
         _cppCommitteeMinSize
         cppCommitteeMaxTermLength
         cppGovActionLifetime
         cppGovActionDeposit
         cppDRepDeposit
         _cppDRepActivity
         _cppMinFeeRefScriptCoinsPerByte ->
            [ assert $ cppMaxBBSize /=. lit (THKD 0)
            , assert $ cppMaxTxSize /=. lit (THKD 0)
            , assert $ cppMaxBHSize /=. lit (THKD 0)
            , assert $ cppMaxValSize /=. lit (THKD 0)
            , assert $ cppCollateralPercentage /=. lit (THKD 0)
            , assert $ cppCommitteeMaxTermLength /=. lit (THKD $ EpochInterval 0)
            , assert $ cppGovActionLifetime /=. lit (THKD $ EpochInterval 0)
            , assert $ cppPoolDeposit /=. lit (THKD mempty)
            , assert $ cppGovActionDeposit /=. lit (THKD mempty)
            , assert $ cppDRepDeposit /=. lit (THKD mempty)
            , match cppEMax $ \epochInterval ->
                lit (EpochInterval 0) <. epochInterval
            ]
