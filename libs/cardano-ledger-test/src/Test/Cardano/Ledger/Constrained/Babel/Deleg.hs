{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the DELEG rule
module Test.Cardano.Ledger.Constrained.Babel.Deleg where

import Cardano.Ledger.CertState
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.UMap (RDPair (..), fromCompact, unUnify)
import qualified Data.Map as Map
import Lens.Micro

import Constrained

import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert)
import Cardano.Ledger.Core (PParams, ppKeyDepositL)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Babel.Instances

dStateSpec ::
  IsConwayUniv fn =>
  Specification fn (DState (BabelEra StandardCrypto))
dStateSpec = constrained $ \ds ->
  match ds $ \rewardMap _futureGenDelegs _genDelegs _rewards ->
    match rewardMap $ \rdMap ptrMap sPoolMap _dRepMap ->
      [ assertExplain ["dom sPoolMap is a subset of dom rdMap"] $ dom_ sPoolMap `subset_` dom_ rdMap
      , assertExplain ["dom ptrMap is empty"] $ dom_ ptrMap ==. mempty
      ]

delegCertSpec ::
  IsConwayUniv fn =>
  PParams (BabelEra StandardCrypto) ->
  DState (BabelEra StandardCrypto) ->
  Specification fn (ConwayDelegCert StandardCrypto)
delegCertSpec pp ds =
  let rewardMap = unUnify $ rewards ds
      delegMap = unUnify $ delegations ds
      zeroReward = (== 0) . fromCompact . rdReward
      depositOf k =
        case fromCompact . rdDeposit <$> Map.lookup k rewardMap of
          Just d | d > 0 -> SJust d
          _ -> SNothing
   in constrained $ \dc ->
        (caseOn dc)
          -- BabelRegCert !(StakeCredential c) !(StrictMaybe Coin)
          (branch $ \_ mc -> mc ==. lit (SJust (pp ^. ppKeyDepositL)))
          -- BabelUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
          ( branch $ \sc mc ->
              [ -- You can only unregister things with 0 reward
                assert $ elem_ sc $ lit (Map.keys $ Map.filter zeroReward rewardMap)
              , assert $ elem_ sc $ lit (Map.keys delegMap)
              , -- The `StrictMaybe` needs to be precisely what is in the delegation map
                reify sc depositOf (==. mc)
              ]
          )
          -- BabelDelegCert !(StakeCredential c) !(Delegatee c)
          (branch $ \sc _ -> member_ sc $ lit (Map.keysSet delegMap))
          -- BabelRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
          ( branch $ \_ _ c ->
              c ==. lit (pp ^. ppKeyDepositL)
          )
