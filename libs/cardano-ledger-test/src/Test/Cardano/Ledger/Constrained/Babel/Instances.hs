{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- RecordWildCards cause name shadowing warnings in ghc-8.10.

-- | This module provides the necessary instances of `HasSpec`
-- and `HasSimpleRep` to write specs for the environments,
-- states, and signals in the conway STS rules.
module Test.Cardano.Ledger.Constrained.Babel.Instances (
  ProposalTree,
  IsConwayUniv,
  gasId_,
  gasCommitteeVotes_,
  gasDRepVotes_,
  gasProposalProcedure_,
  pProcDeposit_,
  pProcGovAction_,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), AuxiliaryDataHash)
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Api (
  GovActionId,
  GovPurposeId (..),
  ProposalProcedure,
  Vote,
  VotingProcedures,
 )
import Cardano.Ledger.Api.Governance (
  ConwayGovState,
  EnactState,
  GovActionState,
  GovRelation (..),
  RatifyState,
 )
import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Rules (BabelUtxoEnv, BatchData)
import Cardano.Ledger.Babel.Scripts ()
import Cardano.Ledger.Babel.Tx (BabelTx)
import Cardano.Ledger.Babel.TxBody
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Governance (
  Committee,
  DRepPulser (..),
  DRepPulsingState,
  GovAction,
  Proposals,
  PulsingSnapshot,
  RatifyEnv,
  RatifySignal,
  TreeMaybe (..),
  proposalsActionsMap,
  toGovRelationTree,
  unsafeMkProposals,
 )
import Cardano.Ledger.Conway.Rules (CertEnv, ConwayGovCertEnv, EnactSignal, GovEnv)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (
  BootstrapWitness,
  KeyHash,
  KeyRole (..),
  WitVKey,
 )
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.Shelley.LedgerState hiding (ptrMap)
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.TxAuxData (Metadatum)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO
import Constrained hiding (Value)
import Data.Coerce
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.OMap.Strict qualified as OMap
import Data.OSet.Strict qualified as SOS
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree
import Data.Word
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Constrained.Conway.Instances (
  DRepPulserTypes,
  GAS,
  IsConwayUniv,
  ProposalTree,
  ProposalsType,
 )
import Test.Cardano.Ledger.Core.Utils

-- TxBody HasSpec instance ------------------------------------------------

-- NOTE: this is a representation of the `BabelTxBody` type. You can't
-- simply use the generics to derive the `SimpleRep` for `BabelTxBody`
-- because the type is memoized. So instead we say that the representation
-- is the same as what you would get from using the `BabelTxBody` pattern.
type BabelTxBodyTypes c =
  '[ Set (TxIn (EraCrypto (BabelEra c)))
   , Set (TxIn (EraCrypto (BabelEra c)))
   , Set (TxIn (EraCrypto (BabelEra c)))
   , StrictSeq (Sized (TxOut (BabelEra c)))
   , StrictMaybe (Sized (TxOut (BabelEra c)))
   , StrictMaybe Coin
   , SOS.OSet (ConwayTxCert (BabelEra c))
   , Withdrawals (EraCrypto (BabelEra c))
   , Coin
   , ValidityInterval
   , Set (KeyHash 'Witness (EraCrypto (BabelEra c)))
   , MultiAsset (EraCrypto (BabelEra c))
   , StrictMaybe (ScriptIntegrityHash (EraCrypto (BabelEra c)))
   , StrictMaybe (AuxiliaryDataHash (EraCrypto (BabelEra c)))
   , StrictMaybe Network
   , VotingProcedures (BabelEra c)
   , SOS.OSet (ProposalProcedure (BabelEra c))
   , StrictMaybe Coin
   , Coin
   , Set (TxId (EraCrypto (BabelEra c)))
   , Set (ScriptHash (EraCrypto (BabelEra c)))
   , StrictSeq (Sized (TxOut (BabelEra c)))
   , Set (TxIn (EraCrypto (BabelEra c)))
   ]
instance
  (IsConwayUniv fn, Crypto c, HasSpec fn (ConwayTxCert (BabelEra c))) =>
  HasSpec fn (BabelTxBody (BabelEra c))

instance Crypto c => HasSimpleRep (BabelTxBody (BabelEra c)) where
  type SimpleRep (BabelTxBody (BabelEra c)) = SOP '["BabelTxBody" ::: BabelTxBodyTypes c]
  toSimpleRep BabelTxBody {..} =
    inject @"BabelTxBody" @'["BabelTxBody" ::: BabelTxBodyTypes c]
      bbtbSpendInputs
      bbtbCollateralInputs
      bbtbReferenceInputs
      bbtbOutputs
      bbtbCollateralReturn
      bbtbTotalCollateral
      bbtbCerts
      bbtbWithdrawals
      bbtbTxfee
      bbtbVldt
      bbtbReqSignerHashes
      bbtbMint
      bbtbScriptIntegrityHash
      bbtbAdHash
      bbtbTxNetworkId
      bbtbVotingProcedures
      bbtbProposalProcedures
      bbtbCurrentTreasuryValue
      bbtbTreasuryDonation
      bbtbSwaps
      bbtbRequireBatchObservers
      bbtbSpendOuts
      bbtbCorInputs
  fromSimpleRep rep =
    algebra @'["BabelTxBody" ::: BabelTxBodyTypes c] rep BabelTxBody

instance HasSimpleRep (PParams (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PParams (BabelEra StandardCrypto))

instance HasSimpleRep (GovEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (GovEnv (BabelEra StandardCrypto))

instance HasSimpleRep (GovActionState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (GovActionState (BabelEra StandardCrypto))

gasId_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (BabelEra StandardCrypto)) ->
  Term fn (GovActionId StandardCrypto)
gasId_ = sel @0

gasCommitteeVotes_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (BabelEra StandardCrypto)) ->
  Term fn (Map (Credential 'HotCommitteeRole StandardCrypto) Vote)
gasCommitteeVotes_ = sel @1

gasDRepVotes_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (BabelEra StandardCrypto)) ->
  Term fn (Map (Credential 'DRepRole StandardCrypto) Vote)
gasDRepVotes_ = sel @2

gasProposalProcedure_ ::
  IsConwayUniv fn =>
  Term fn (GovActionState (BabelEra StandardCrypto)) ->
  Term fn (ProposalProcedure (BabelEra StandardCrypto))
gasProposalProcedure_ = sel @4

instance HasSimpleRep (Proposals (BabelEra StandardCrypto)) where
  type
    SimpleRep (Proposals (BabelEra StandardCrypto)) =
      SOP '["Proposals" ::: ProposalsType BabelEra]
  toSimpleRep props =
    inject @"Proposals" @'["Proposals" ::: ProposalsType BabelEra]
      (buildProposalTree $ coerce grPParamUpdate)
      (buildProposalTree $ coerce grHardFork)
      (buildProposalTree $ coerce grCommittee)
      (buildProposalTree $ coerce grConstitution)
      (Map.elems $ Map.withoutKeys idMap treeKeys)
    where
      GovRelation {..} = toGovRelationTree props
      idMap = proposalsActionsMap props

      treeKeys =
        foldMap
          keys
          [ coerce grPParamUpdate
          , coerce grHardFork
          , coerce grCommittee
          , coerce grConstitution
          ]

      buildProposalTree ::
        TreeMaybe (GovActionId StandardCrypto) -> ProposalTree BabelEra
      buildProposalTree (TreeMaybe (Node mId cs)) = (mId, map buildTree cs)

      buildTree :: Tree (StrictMaybe (GovActionId StandardCrypto)) -> Tree (GAS (BabelEra StandardCrypto))
      buildTree (Node (SJust gid) cs) | Just gas <- Map.lookup gid idMap = Node gas (map buildTree cs)
      buildTree _ =
        error "toSimpleRep @Proposals: toGovRelationTree returned trees with Nothing nodes below the root"

      keys :: TreeMaybe (GovActionId StandardCrypto) -> Set (GovActionId StandardCrypto)
      keys (TreeMaybe t) = foldMap (strictMaybe mempty Set.singleton) t

  fromSimpleRep rep =
    algebra @'["Proposals" ::: ProposalsType BabelEra]
      rep
      $ \(rPPUp, ppupTree) (rHF, hfTree) (rCom, comTree) (rCon, conTree) others ->
        let root = GovRelation (coerce rPPUp) (coerce rHF) (coerce rCom) (coerce rCon)
            -- TODO: note, this doesn't roundtrip and the distribution is a bit iffy. See the TODO
            -- above for ideas on how to deal with this.
            oMap = foldMap (foldMap mkOMap) [ppupTree, hfTree, comTree, conTree] <> OMap.fromFoldable others
         in unsafeMkProposals root oMap
    where
      mkOMap (Node a ts) = a OMap.<| foldMap mkOMap ts

instance IsConwayUniv fn => HasSpec fn (Proposals (BabelEra StandardCrypto))

instance HasSimpleRep (EnactSignal (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EnactSignal (BabelEra StandardCrypto))

instance HasSimpleRep (EnactState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EnactState (BabelEra StandardCrypto))

instance HasSimpleRep (Committee (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (Committee (BabelEra StandardCrypto))

instance HasSimpleRep (RatifyEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifyEnv (BabelEra StandardCrypto))

instance HasSimpleRep (RatifyState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifyState (BabelEra StandardCrypto))

instance HasSimpleRep (RatifySignal (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (RatifySignal (BabelEra StandardCrypto))

instance HasSimpleRep (ConwayGovCertEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (ConwayGovCertEnv (BabelEra StandardCrypto))

instance HasSimpleRep (PoolEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PoolEnv (BabelEra StandardCrypto))

instance HasSimpleRep (CertEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (CertEnv (BabelEra StandardCrypto))

instance HasSimpleRep (EpochState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (EpochState (BabelEra StandardCrypto))

instance HasSimpleRep (LedgerState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (LedgerState (BabelEra StandardCrypto))

instance HasSimpleRep (UTxOState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (UTxOState (BabelEra StandardCrypto))

instance HasSimpleRep (UTxO (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (UTxO (BabelEra StandardCrypto))

instance HasSimpleRep (ConwayGovState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (ConwayGovState (BabelEra StandardCrypto))

instance HasSimpleRep (DRepPulsingState (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (DRepPulsingState (BabelEra StandardCrypto))

instance HasSimpleRep (PulsingSnapshot (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (PulsingSnapshot (BabelEra StandardCrypto))

instance
  HasSimpleRep
    (DRepPulser (BabelEra StandardCrypto) Identity (RatifyState (BabelEra StandardCrypto)))
  where
  type
    SimpleRep (DRepPulser (BabelEra StandardCrypto) Identity (RatifyState (BabelEra StandardCrypto))) =
      SOP '["DRepPulser" ::: DRepPulserTypes BabelEra]
  toSimpleRep DRepPulser {..} =
    inject @"DRepPulser" @'["DRepPulser" ::: DRepPulserTypes BabelEra]
      dpPulseSize
      dpUMap
      dpIndex
      dpStakeDistr
      dpStakePoolDistr
      dpDRepDistr
      dpDRepState
      dpCurrentEpoch
      dpCommitteeState
      dpEnactState
      dpProposals
  fromSimpleRep rep =
    algebra @'["DRepPulser" ::: DRepPulserTypes BabelEra]
      rep
      $ \ps um b sd spd dd ds ce cs es p ->
        DRepPulser ps um b sd spd dd ds ce cs es p testGlobals
instance
  IsConwayUniv fn =>
  HasSpec fn (DRepPulser (BabelEra StandardCrypto) Identity (RatifyState (BabelEra StandardCrypto)))

instance HasSimpleRep (BatchData (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (BatchData (BabelEra StandardCrypto))

instance HasSimpleRep (BabelUtxoEnv (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (BabelUtxoEnv (BabelEra StandardCrypto))

instance HasSimpleRep (AlonzoTx (BabelEra StandardCrypto))
instance
  (IsConwayUniv fn, HasSpec fn (ConwayTxCert (BabelEra StandardCrypto))) =>
  HasSpec fn (AlonzoTx (BabelEra StandardCrypto))

instance HasSimpleRep (BabelTx (BabelEra StandardCrypto))
instance IsConwayUniv fn => HasSpec fn (BabelTx (BabelEra StandardCrypto))

-- NOTE: we don't generate or talk about plutus scripts (yet!)
type AlonzoTxAuxDataTypes =
  '[ Map Word64 Metadatum
   , StrictSeq (Timelock (BabelEra StandardCrypto))
   ]
instance HasSimpleRep (AlonzoTxAuxData (BabelEra StandardCrypto)) where
  type
    SimpleRep (AlonzoTxAuxData (BabelEra StandardCrypto)) =
      SOP '["AlonzoTxOutData" ::: AlonzoTxAuxDataTypes]
  toSimpleRep (AlonzoTxAuxData metaMap tsSeq _) =
    inject @"AlonzoTxAuxData" @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes]
      metaMap
      tsSeq
  fromSimpleRep rep =
    algebra @'["AlonzoTxAuxData" ::: AlonzoTxAuxDataTypes] rep $
      \metaMap tsSeq -> AlonzoTxAuxData metaMap tsSeq mempty
instance IsConwayUniv fn => HasSpec fn (AlonzoTxAuxData (BabelEra StandardCrypto))

type AlonzoTxWitsTypes =
  '[ Set (WitVKey 'Witness StandardCrypto)
   , Set (BootstrapWitness StandardCrypto)
   ]
instance HasSimpleRep (AlonzoTxWits (BabelEra StandardCrypto)) where
  type
    SimpleRep (AlonzoTxWits (BabelEra StandardCrypto)) =
      SOP '["AlonzoTxWits" ::: AlonzoTxWitsTypes]
  toSimpleRep (AlonzoTxWits vkeyWits bootstrapWits _ _ _) =
    inject @"AlonzoTxWits" @'["AlonzoTxWits" ::: AlonzoTxWitsTypes]
      vkeyWits
      bootstrapWits
  fromSimpleRep rep =
    algebra @'["AlonzoTxWits" ::: AlonzoTxWitsTypes] rep $
      \vkeyWits bootstrapWits -> AlonzoTxWits vkeyWits bootstrapWits mempty (TxDats mempty) (Redeemers mempty)
instance IsConwayUniv fn => HasSpec fn (AlonzoTxWits (BabelEra StandardCrypto))

pProcDeposit_ ::
  IsConwayUniv fn =>
  Term fn (ProposalProcedure (BabelEra StandardCrypto)) ->
  Term fn Coin
pProcDeposit_ = sel @0

pProcGovAction_ ::
  IsConwayUniv fn =>
  Term fn (ProposalProcedure (BabelEra StandardCrypto)) ->
  Term fn (GovAction (BabelEra StandardCrypto))
pProcGovAction_ = sel @2