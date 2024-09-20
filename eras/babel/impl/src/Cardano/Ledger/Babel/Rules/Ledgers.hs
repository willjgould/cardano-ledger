{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Ledgers (BabelLEDGERS, BabelLedgersPredFailure (..)) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure)
import Cardano.Ledger.Babel.Era (BabelEra, BabelLEDGER, BabelLEDGERS)
import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerEvent, BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Pool ()
import Cardano.Ledger.Babel.Rules.Swaps (
  BabelSwapsPredFailure,
 )
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Babel.Tx (BabelEraTx (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Binary.Decoding (DecCBOR (..))
import Cardano.Ledger.Binary.Encoding (EncCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.API (LedgerEnv (LedgerEnv), ShelleyLedgersEnv (LedgersEnv))
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.Rules (ShelleyLedgersEvent (..))
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.State.Transition (
  Embed (wrapEvent, wrapFailed),
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Default.Class (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

newtype BabelLedgersPredFailure era
  = LedgerFailure (PredicateFailure (EraRule "LEDGER" era)) -- Subtransition Failures
  deriving (Generic)

instance
  (Era era, EncCBOR (PredicateFailure (EraRule "LEDGER" era))) =>
  EncCBOR (BabelLedgersPredFailure era)
  where
  encCBOR =
    encode . \case
      LedgerFailure x -> Sum (LedgerFailure @era) 0 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (BabelLedgersPredFailure era)
  where
  decCBOR = decode (Summands "BabelLedgersPredFailure" dec)
    where
      dec 0 = SumD LedgerFailure <! From
      dec n = Invalid n

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Show (BabelLedgersPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  Eq (BabelLedgersPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  NoThunks (BabelLedgersPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  NFData (BabelLedgersPredFailure era)

deriving instance (Era era, Eq (Event (EraRule "LEDGER" era))) => Eq (ShelleyLedgersEvent era)

type instance EraRuleFailure "LEDGERS" (BabelEra c) = BabelLedgersPredFailure (BabelEra c)

type instance EraRuleEvent "LEDGERS" (BabelEra c) = ShelleyLedgersEvent (BabelEra c)

instance InjectRuleFailure "LEDGERS" BabelLedgersPredFailure (BabelEra c)

instance InjectRuleFailure "LEDGERS" BabelSwapsPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelLedgerPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = LedgerFailure . injectFailure

instance
  ( Era era
  , Embed (EraRule "LEDGER" era) (BabelLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Default (LedgerState era)
  , EraTx era
  , BabelEraTx era
  ) =>
  STS (BabelLEDGERS era)
  where
  type State (BabelLEDGERS era) = LedgerState era
  type Signal (BabelLEDGERS era) = Seq (Tx era)
  type Environment (BabelLEDGERS era) = ShelleyLedgersEnv era
  type BaseM (BabelLEDGERS era) = ShelleyBase
  type PredicateFailure (BabelLEDGERS era) = BabelLedgersPredFailure era
  type Event (BabelLEDGERS era) = ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

{- CIP-0118#LEDGERS-rule

Jump to CIP-0118#LEDGER-rule to continue... -}
ledgersTransition ::
  forall era.
  ( Embed (EraRule "LEDGER" era) (BabelLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (BabelLEDGERS era)
ledgersTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  -- let txMap = (Map.fromList . toList) . fmap (\tx -> (txIdTx tx, tx)) $ txwits
  --     entriesMatching :: Ord k => Map.Map k v -> Seq k -> Seq v
  --     entriesMatching m = Seq.fromList . Map.elems . Map.restrictKeys m . (Set.fromList . toList)
  foldM
    ( \ !ls' (ix, tx) ->
        trans @(EraRule "LEDGER" era) $
          TRC (LedgerEnv slot ix pp account, ls', tx)
    )
    ls
    $ zip [minBound ..]
    $ toList txwits

instance
  ( Era era
  , STS (BabelLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ BabelLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ BabelLedgerEvent era
  ) =>
  Embed (BabelLEDGER era) (BabelLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent