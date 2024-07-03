{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel (
  Babel,
  BabelEra,
)
where

import Cardano.Crypto.DSIGN (Signable)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Governance ()
import Cardano.Ledger.Babel.Rules ()
import Cardano.Ledger.Babel.Rules.Gov ()
import Cardano.Ledger.Babel.Transition ()
import Cardano.Ledger.Babel.Translation ()
import Cardano.Ledger.Babel.Tx ()
import Cardano.Ledger.Babel.TxInfo ()
import Cardano.Ledger.Babel.TxOut ()
import Cardano.Ledger.Babel.UTxO ()
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Crypto (Crypto (DSIGN), StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Cardano.Ledger.Shelley.API (
  ApplyBlock,
  ApplyTx (..),
  Block,
  BlockTransitionError (BlockTransitionError),
  Globals,
 )
import Cardano.Ledger.Shelley.API.Genesis (CanStartFromGenesis (..))
import Cardano.Ledger.Shelley.API.Mempool (
  ApplyTxError (ApplyTxError),
  extractTx,
 )
import Control.Arrow (ArrowChoice (right), left)
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition (
  ApplySTSOpts,
  BaseM,
  Environment,
  EventReturnType,
  EventReturnTypeRep,
  STS,
  Signal,
  TRC (TRC),
  mapEventReturn,
  reapplySTS,
 )

import qualified Cardano.Crypto.Hash.Class
import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Transition ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxInfo ()
import Cardano.Ledger.Babbage.UTxO ()
import Cardano.Ledger.Babel.LedgerState.Types (LedgerStateTemp, fromLedgerState, toLedgerState)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Shelley.API.Types (NewEpochState)
import Cardano.Ledger.Shelley.API.Validation (ApplyBlock (..))
import Cardano.Ledger.Shelley.LedgerState (HasLedgerState (from), curPParamsEpochStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import qualified Cardano.Ledger.Shelley.Rules as STS
import Control.State.Transition.Extended (applySTSOptsEither)
import Lens.Micro ((^.))

type Babel = BabelEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , DSignable c (Hash c EraIndependentRequiredTxs)
  , -- TODO WG figure out what you've done wrong to introduce this constraint
    Signable (DSIGN c) (Cardano.Crypto.Hash.Class.Hash c EraIndependentTxBody)
  ) =>
  ApplyBlock (BabelEra c)
  where
  applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError (BabelEra c)) m) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState (BabelEra c) ->
    Block (BHeaderView (EraCrypto (BabelEra c))) (BabelEra c) ->
    m (EventReturnType ep (EraRule "BBODY" (BabelEra c)) (NewEpochState (BabelEra c)))
  applyBlockOpts opts globals state blk =
    liftEither
      . left BlockTransitionError
      . right
        ( mapEventReturn @ep @(EraRule "BBODY" (BabelEra c)) $
            updateNewEpochState state
        )
      $ res
    where
      res =
        flip runReader globals
          . applySTSOptsEither @(EraRule "BBODY" (BabelEra c))
            opts
          $ TRC (mkBbodyEnv state, bbs, blk)
      bbs =
        STS.BbodyState
          (fromLedgerState $ LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)
  reapplyBlock ::
    forall era.
    ( STS.State (EraRule "LEDGERS" era) ~ LedgerStateTemp era
    , STS.State (EraRule "BBODY" era) ~ STS.ShelleyBbodyState era
    , BaseM (EraRule "BBODY" era) ~ ShelleyBase
    , Environment (EraRule "BBODY" era) ~ STS.BbodyEnv era
    , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
    , EraGov era
    , STS (EraRule "BBODY" era)
    ) =>
    Globals ->
    NewEpochState era ->
    Block (BHeaderView (EraCrypto era)) era ->
    NewEpochState era
  reapplyBlock globals state blk =
    updateNewEpochState state res
    where
      res =
        flip runReader globals . reapplySTS @(EraRule "BBODY" era) $
          TRC (mkBbodyEnv state, bbs, blk)
      bbs =
        STS.BbodyState
          (fromLedgerState $ LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)

updateNewEpochState ::
  (LedgerStateTemp era ~ STS.State (EraRule "LEDGERS" era), EraGov era) =>
  NewEpochState era ->
  STS.ShelleyBbodyState era ->
  NewEpochState era
updateNewEpochState ss (STS.BbodyState ls bcur) =
  LedgerState.updateNES ss bcur (toLedgerState ls)

mkBbodyEnv ::
  EraGov era =>
  NewEpochState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodyPp = nesEs ^. curPParamsEpochStateL
      , STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , DSignable c (Hash c EraIndependentRequiredTxs)
  ) =>
  ApplyTx (BabelEra c)
  where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(EraRule "LEDGER" (BabelEra c))
            $ TRC (env, from state, extractTx vtx)
     in liftEither . left ApplyTxError . right from $ res

instance Crypto c => CanStartFromGenesis (BabelEra c) where
  type AdditionalGenesisConfig (BabelEra c) = BabelGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Babel to work"

instance Crypto c => RunConwayRatify (BabelEra c)
