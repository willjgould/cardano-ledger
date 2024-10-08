{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.TxCert (
  -- BabelTxCert (..),
  BabelTxCertUpgradeError (..),
  Delegatee (..),
  BabelEraTxCert,
  -- getScriptWitnessBabelTxCert,
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern AuthCommitteeHotKeyTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RegDRepTxCert,
  pattern UnRegDRepTxCert,
  pattern UpdateDRepTxCert,
)
where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.PParams ()
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Anchor)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepDepositL)
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (ConwayDelegCert, ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert),
  ConwayEraTxCert (..),
  ConwayGovCert (
    ConwayAuthCommitteeHotKey,
    ConwayRegDRep,
    ConwayResignCommitteeColdKey,
    ConwayUnRegDRep,
    ConwayUpdateDRep
  ),
  ConwayTxCert (ConwayTxCertDeleg, ConwayTxCertGov, ConwayTxCertPool),
  Delegatee (..),
  getScriptWitnessConwayTxCert,
  getVKeyWitnessConwayTxCert,
 )
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.TxCert (
  shelleyTotalDepositsTxCerts,
  shelleyTotalRefundsTxCerts,
 )
import Cardano.Ledger.Val (Val (..))
import Data.Foldable (foldMap', foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (getSum))
import Lens.Micro

data BabelTxCertUpgradeError
  = MirTxCertExpunged
  | GenesisDelegTxCertExpunged
  deriving (Eq, Show)

instance Crypto c => EraTxCert (BabelEra c) where
  type TxCert (BabelEra c) = ConwayTxCert (BabelEra c)

  type TxCertUpgradeError (BabelEra c) = BabelTxCertUpgradeError

  upgradeTxCert = \case
    RegPoolTxCert poolParams -> Right $ RegPoolTxCert poolParams
    RetirePoolTxCert poolId epochNo -> Right $ RetirePoolTxCert poolId epochNo
    RegTxCert cred -> Right $ RegTxCert cred
    UnRegTxCert cred -> Right $ UnRegTxCert cred
    DelegStakeTxCert cred poolId -> Right $ DelegStakeTxCert cred poolId
    -- Using wildcard here instead of a pattern match on GenesisDelegTxCert in order to
    -- workaround ghc-8.10 disrespecting the completeness pragma.
    _ -> Left GenesisDelegTxCertExpunged

  getVKeyWitnessTxCert = getVKeyWitnessConwayTxCert

  getScriptWitnessTxCert = getScriptWitnessConwayTxCert

  mkRegPoolTxCert = ConwayTxCertPool . RegPool

  getRegPoolTxCert (ConwayTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ConwayTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ConwayTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

  lookupRegStakeTxCert = \case
    RegTxCert c -> Just c
    RegDepositTxCert c _ -> Just c
    RegDepositDelegTxCert c _ _ -> Just c
    _ -> Nothing
  lookupUnRegStakeTxCert = \case
    UnRegTxCert c -> Just c
    UnRegDepositTxCert c _ -> Just c
    _ -> Nothing

  getTotalRefundsTxCerts = babelTotalRefundsTxCerts

  getTotalDepositsTxCerts = babelTotalDepositsTxCerts

instance Crypto c => ShelleyEraTxCert (BabelEra c) where
  mkRegTxCert c = ConwayTxCertDeleg $ ConwayRegCert c SNothing

  getRegTxCert (ConwayTxCertDeleg (ConwayRegCert c _)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert c = ConwayTxCertDeleg $ ConwayUnRegCert c SNothing

  getUnRegTxCert (ConwayTxCertDeleg (ConwayUnRegCert c _)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ConwayTxCertDeleg $ ConwayDelegCert c (DelegStake kh)

  getDelegStakeTxCert (ConwayTxCertDeleg (ConwayDelegCert c (DelegStake kh))) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = notSupportedInThisEra
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = notSupportedInThisEra
  getMirTxCert = const Nothing

instance Crypto c => ConwayEraTxCert (BabelEra c) where
  mkRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayRegCert cred $ SJust c

  getRegDepositTxCert (ConwayTxCertDeleg (ConwayRegCert cred (SJust c))) = Just (cred, c)
  getRegDepositTxCert _ = Nothing

  mkUnRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayUnRegCert cred (SJust c)
  getUnRegDepositTxCert (ConwayTxCertDeleg (ConwayUnRegCert cred (SJust c))) = Just (cred, c)
  getUnRegDepositTxCert _ = Nothing

  mkDelegTxCert cred d = ConwayTxCertDeleg $ ConwayDelegCert cred d
  getDelegTxCert (ConwayTxCertDeleg (ConwayDelegCert cred d)) = Just (cred, d)
  getDelegTxCert _ = Nothing

  mkRegDepositDelegTxCert cred d c = ConwayTxCertDeleg $ ConwayRegDelegCert cred d c
  getRegDepositDelegTxCert (ConwayTxCertDeleg (ConwayRegDelegCert cred d c)) = Just (cred, d, c)
  getRegDepositDelegTxCert _ = Nothing

  mkAuthCommitteeHotKeyTxCert ck hk = ConwayTxCertGov $ ConwayAuthCommitteeHotKey ck hk
  getAuthCommitteeHotKeyTxCert (ConwayTxCertGov (ConwayAuthCommitteeHotKey ck hk)) = Just (ck, hk)
  getAuthCommitteeHotKeyTxCert _ = Nothing

  mkResignCommitteeColdTxCert ck a = ConwayTxCertGov $ ConwayResignCommitteeColdKey ck a
  getResignCommitteeColdTxCert (ConwayTxCertGov (ConwayResignCommitteeColdKey ck a)) = Just (ck, a)
  getResignCommitteeColdTxCert _ = Nothing

  mkRegDRepTxCert cred deposit mAnchor = ConwayTxCertGov $ ConwayRegDRep cred deposit mAnchor
  getRegDRepTxCert = \case
    ConwayTxCertGov (ConwayRegDRep cred deposit mAnchor) -> Just (cred, deposit, mAnchor)
    _ -> Nothing

  mkUnRegDRepTxCert cred deposit = ConwayTxCertGov $ ConwayUnRegDRep cred deposit
  getUnRegDRepTxCert = \case
    ConwayTxCertGov (ConwayUnRegDRep cred deposit) -> Just (cred, deposit)
    _ -> Nothing

  mkUpdateDRepTxCert cred mAnchor = ConwayTxCertGov $ ConwayUpdateDRep cred mAnchor
  getUpdateDRepTxCert = \case
    ConwayTxCertGov (ConwayUpdateDRep cred mAnchor) -> Just (cred, mAnchor)
    _ -> Nothing

class ConwayEraTxCert era => BabelEraTxCert era

instance Crypto c => BabelEraTxCert (BabelEra c)

pattern RegDepositTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositTxCert cred c <- (getRegDepositTxCert -> Just (cred, c))
  where
    RegDepositTxCert cred c = mkRegDepositTxCert cred c

pattern UnRegDepositTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern UnRegDepositTxCert cred c <- (getUnRegDepositTxCert -> Just (cred, c))
  where
    UnRegDepositTxCert cred c = mkUnRegDepositTxCert cred c

pattern DelegTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  TxCert era
pattern DelegTxCert cred d <- (getDelegTxCert -> Just (cred, d))
  where
    DelegTxCert cred d = mkDelegTxCert cred d

pattern RegDepositDelegTxCert ::
  BabelEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositDelegTxCert cred d c <- (getRegDepositDelegTxCert -> Just (cred, d, c))
  where
    RegDepositDelegTxCert cred d c = mkRegDepositDelegTxCert cred d c

pattern AuthCommitteeHotKeyTxCert ::
  BabelEraTxCert era =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  Credential 'HotCommitteeRole (EraCrypto era) ->
  TxCert era
pattern AuthCommitteeHotKeyTxCert ck hk <- (getAuthCommitteeHotKeyTxCert -> Just (ck, hk))
  where
    AuthCommitteeHotKeyTxCert ck hk = mkAuthCommitteeHotKeyTxCert ck hk

pattern ResignCommitteeColdTxCert ::
  BabelEraTxCert era =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  TxCert era
pattern ResignCommitteeColdTxCert ck a <- (getResignCommitteeColdTxCert -> Just (ck, a))
  where
    ResignCommitteeColdTxCert ck = mkResignCommitteeColdTxCert ck

pattern RegDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  Coin ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  TxCert era
pattern RegDRepTxCert cred deposit mAnchor <- (getRegDRepTxCert -> Just (cred, deposit, mAnchor))
  where
    RegDRepTxCert cred deposit mAnchor = mkRegDRepTxCert cred deposit mAnchor

pattern UnRegDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  Coin ->
  TxCert era
pattern UnRegDRepTxCert cred deposit <- (getUnRegDRepTxCert -> Just (cred, deposit))
  where
    UnRegDRepTxCert cred deposit = mkUnRegDRepTxCert cred deposit

pattern UpdateDRepTxCert ::
  BabelEraTxCert era =>
  Credential 'DRepRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  TxCert era
pattern UpdateDRepTxCert cred mAnchor <- (getUpdateDRepTxCert -> Just (cred, mAnchor))
  where
    UpdateDRepTxCert cred mAnchor = mkUpdateDRepTxCert cred mAnchor

{-# COMPLETE
  RegPoolTxCert
  , RetirePoolTxCert
  , RegTxCert
  , UnRegTxCert
  , RegDepositTxCert
  , UnRegDepositTxCert
  , DelegTxCert
  , RegDepositDelegTxCert
  , AuthCommitteeHotKeyTxCert
  , ResignCommitteeColdTxCert
  , RegDRepTxCert
  , UnRegDRepTxCert
  , UpdateDRepTxCert
  #-}

-- | Determine the total deposit amount needed from a TxBody.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration. It is even possible for a single
-- transaction to have multiple pool registration for the same pool, so as
-- we process pool registrations, we must keep track of those that are already
-- registered, so we do not add a Deposit for the same pool twice.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
babelTotalDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  f (TxCert era) ->
  Coin
babelTotalDepositsTxCerts pp isRegPoolRegistered certs =
  shelleyTotalDepositsTxCerts pp isRegPoolRegistered certs
    <+> babelDRepDepositsTxCerts pp certs

babelDRepDepositsTxCerts ::
  (ConwayEraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  f (TxCert era) ->
  Coin
babelDRepDepositsTxCerts pp certs = nDReps <×> depositPerDRep
  where
    nDReps = getSum @Int (foldMap' (\case RegDRepTxCert {} -> 1; _ -> 0) certs)
    depositPerDRep = pp ^. ppDRepDepositL

-- | Compute the key deregistration refunds in a transaction
babelTotalRefundsTxCerts ::
  (EraPParams era, Foldable f, BabelEraTxCert era) =>
  PParams era ->
  -- | Function that can lookup current deposit, in case when the Staking credential is registered.
  (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
  -- | Function that can lookup current deposit, in case when the DRep credential is registered.
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
babelTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit certs =
  shelleyTotalRefundsTxCerts pp lookupStakingDeposit certs
    <+> babelDRepRefundsTxCerts lookupDRepDeposit certs

-- | Compute the Refunds from a TxBody, given a function that computes a partial Coin for
-- known Credentials.
babelDRepRefundsTxCerts ::
  (Foldable f, BabelEraTxCert era) =>
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
babelDRepRefundsTxCerts lookupDRepDeposit = snd . foldl' go (Map.empty, Coin 0)
  where
    go accum@(!drepRegsInTx, !totalRefund) = \case
      RegDRepTxCert cred deposit _ ->
        -- Track registrations
        (Map.insert cred deposit drepRegsInTx, totalRefund)
      UnRegDRepTxCert cred _
        -- DRep previously registered in the same tx.
        | Just deposit <- Map.lookup cred drepRegsInTx ->
            (Map.delete cred drepRegsInTx, totalRefund <+> deposit)
        -- DRep previously registered in some other tx.
        | Just deposit <- lookupDRepDeposit cred -> (drepRegsInTx, totalRefund <+> deposit)
      _ -> accum
