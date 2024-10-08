{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Babel (
  SpecTranslate (..),
  SpecTranslationError,
) where

import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Rules (BabelUtxoEnv, BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Scripts (BabelPlutusPurpose (..))
import Cardano.Ledger.Babel.Tx
import Cardano.Ledger.Shelley.Rules (Identity)
import Control.State.Transition.Extended (STS (..))
import Lens.Micro
import qualified Lib as Agda
import Test.Cardano.Ledger.Babel.TreeDiff (ToExpr (..))
import Test.Cardano.Ledger.Conformance (
  SpecTransM,
  SpecTranslate (..),
  SpecTranslationError,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (OpaqueErrorString (OpaqueErrorString))

instance
  ( SpecRep (PParams era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (BabelUtxoEnv era)
  where
  type SpecRep (BabelUtxoEnv era) = Agda.UTxOEnv

  toSpecRep _x = undefined -- TODO when Agda is done

instance SpecTranslate ctx (BabelPlutusPurpose AsIx era) where
  type SpecRep (BabelPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    BabelSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    BabelMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    BabelCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    BabelRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)
    BabelVoting (AsIx i) -> pure (Agda.Vote, toInteger i)
    BabelProposing (AsIx i) -> pure (Agda.Propose, toInteger i)
    BabelSpendOut (AsIx _) -> pure undefined -- TODO WG (Agda.SpendOut, toInteger i)
    BabelBatchObs (AsIx _) -> pure undefined -- TODO WG (Agda.BatchObs, toInteger i)

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (TxCert era)
  ) =>
  Tx era ->
  SpecTransM ctx Agda.TxBody
toAgdaTxBody tx =
  Agda.MkTxBody
    <$> toSpecRep (tx ^. bodyTxL . inputsTxBodyL)
    <*> (zip [0 ..] <$> toSpecRep (tx ^. bodyTxL . outputsTxBodyL))
    <*> toSpecRep (tx ^. bodyTxL . feeTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . vldtTxBodyL)
    <*> pure (tx ^. sizeTxF)
    <*> toSpecRep (txIdTx tx)
    <*> toSpecRep (tx ^. bodyTxL . collateralInputsTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . reqSignerHashesTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . certsTxBodyL)

-- TODO WG When agda is ready

instance
  ( SpecTranslate ctx (TxWits era)
  , SpecTranslate ctx (TxAuxData era)
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (TxCert era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , Tx era ~ BabelTx era
  , EraTx era
  , AlonzoEraTxBody era
  ) =>
  SpecTranslate ctx (BabelTx era)
  where
  type SpecRep (BabelTx era) = Agda.Tx

  toSpecRep _tx = undefined -- TODO WG

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  SpecTranslate ctx (BabelUtxoPredFailure era)
  where
  type SpecRep (BabelUtxoPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e
