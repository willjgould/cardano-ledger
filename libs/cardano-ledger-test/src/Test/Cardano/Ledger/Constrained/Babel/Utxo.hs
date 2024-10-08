{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the UTXO rule
module Test.Cardano.Ledger.Constrained.Babel.Utxo where

import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.Babel.PParams
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UTxO
import Data.Foldable
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Word
import Lens.Micro

import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core (EraTx (..), ppMaxCollateralInputsL)
import Cardano.Ledger.Babel.Rules (BabelUtxoEnv (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Babel.Instances
import Test.Cardano.Ledger.Constrained.Babel.PParams
import Test.Cardano.Ledger.Constrained.Conway.Instances hiding (pProcDeposit_)

utxoEnvSpec :: IsConwayUniv fn => Specification fn (BabelUtxoEnv (BabelEra StandardCrypto))
utxoEnvSpec =
  constrained $ \utxoEnv ->
    match utxoEnv $
      \_ueSlot
       uePParams
       _ueCertState
       _ueBatchObservers
       _ueBatchData ->
          [ satisfies uePParams pparamsSpec
          , match uePParams $ \cpp ->
              match cpp $
                \_cppMinFeeA
                 _cppMinFeeB
                 _cppMaxBBSize
                 cppMaxTxSize
                 _cppMaxBHSize
                 _cppKeyDeposit
                 _cppPoolDeposit
                 _cppEMax
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
                 _cppMaxValSize
                 _cppCollateralPercentage
                 _cppMaxCollateralInputs
                 _cppPoolVotingThresholds
                 _cppDRepVotingThresholds
                 _cppCommitteeMinSize
                 _cppCommitteeMaxTermLength
                 _cppGovActionLifetime
                 _cppGovActionDeposit
                 _cppDRepDeposit
                 _cppDRepActivity
                 _cppMinFeeRefScriptCoinsPerByte ->
                    -- NOTE: this is for testing only! We should figure out a nicer way
                    -- of splitting generation and checking constraints here!
                    [ assert $ lit (THKD 3000) ==. cppMaxTxSize
                    ]
          ]

utxoStateSpec ::
  IsConwayUniv fn =>
  BabelUtxoEnv (BabelEra StandardCrypto) ->
  Specification fn (UTxOState (BabelEra StandardCrypto))
utxoStateSpec _env =
  constrained $ \utxoState ->
    match utxoState $
      \utxosUtxo
       _utxosDeposited
       _utxosFees
       _utxosGovState
       _utxosStakeDistr
       _utxosDonation ->
          [ assert $ utxosUtxo /=. lit mempty
          , match utxosUtxo $ \utxoMap ->
              forAll (rng_ utxoMap) correctAddrAndWFCoin
          ]

utxoTxSpec ::
  IsConwayUniv fn =>
  BabelUtxoEnv (BabelEra StandardCrypto) ->
  UTxOState (BabelEra StandardCrypto) ->
  Specification fn (Tx (BabelEra StandardCrypto))
utxoTxSpec env st =
  constrained $ \tx ->
    match tx $ \bdy _wits isValid _auxData _swaps ->
      [ match isValid assert
      , match bdy $
          \bbtbSpendInputs
           bbtbCollateralInputs
           _bbtbReferenceInputs
           bbtbOutputs
           bbtbCollateralReturn
           _bbtbTotalCollateral
           _bbtbCerts
           bbtbWithdrawals
           bbtbTxfee
           bbtbVldt
           _bbtbReqSignerHashes
           _bbtbMint
           _bbtbScriptIntegrityHash
           _bbtbAdHash
           bbtbTxNetworkId
           _bbtbVotingProcedures
           bbtbProposalProcedures
           _bbtbCurrentTreasuryValue
           bbtbTreasuryDonation
           _bbtbSwaps
           _bbtbBatchObs
           _bbtbSpendOuts
           _bbtbCorIns ->
              [ assert $ bbtbSpendInputs /=. lit mempty
              , assert $ bbtbSpendInputs `subset_` lit (Map.keysSet $ unUTxO $ utxosUtxo st)
              , match bbtbWithdrawals $ \withdrawalMap ->
                  forAll' (dom_ withdrawalMap) $ \net _ ->
                    net ==. lit Testnet
              , -- TODO: we need to do this for collateral as well?
                match bbtbProposalProcedures $ \proposalsList ->
                  match bbtbOutputs $ \outputList ->
                    [ (reify bbtbSpendInputs)
                        ( \actualInputs ->
                            fold
                              [ c | i <- Set.toList actualInputs, BabbageTxOut _ (MaryValue c _) _ _ <- maybeToList . txinLookup i . utxosUtxo $ st
                              ]
                        )
                        $ \totalValueConsumed ->
                          [ let outputSum =
                                  foldMap_
                                    (maryValueCoin_ . txOutVal_ . sizedValue_)
                                    outputList
                                depositSum =
                                  foldMap_
                                    pProcDeposit_
                                    proposalsList
                             in outputSum + depositSum + bbtbTxfee + bbtbTreasuryDonation ==. totalValueConsumed
                          ]
                    , forAll outputList (flip onSized correctAddrAndWFCoin)
                    ]
              , match bbtbVldt $ \before after ->
                  [ onJust' before (<=. lit (bueSlot env))
                  , onJust' after (lit (bueSlot env) <.)
                  ]
              , onJust' bbtbTxNetworkId (==. lit Testnet)
              , onJust' bbtbCollateralReturn $ flip onSized correctAddrAndWFCoin
              , assert $
                  size_ bbtbCollateralInputs <=. lit (fromIntegral $ buePParams env ^. ppMaxCollateralInputsL)
              ]
      ]

correctAddrAndWFCoin ::
  IsConwayUniv fn =>
  Term fn (TxOut (BabelEra StandardCrypto)) ->
  Pred fn
correctAddrAndWFCoin txOut =
  match txOut $ \addr v _ _ ->
    [ match v $ \c -> [0 <. c, c <=. fromIntegral (maxBound :: Word64)]
    , (caseOn addr)
        (branch $ \n _ _ -> n ==. lit Testnet)
        ( branch $ \bootstrapAddr ->
            match bootstrapAddr $ \_ nm _ ->
              (caseOn nm)
                (branch $ \_ -> False)
                (branch $ \_ -> True)
        )
    ]
