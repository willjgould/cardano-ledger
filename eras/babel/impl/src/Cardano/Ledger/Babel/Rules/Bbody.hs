{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Rules.Bbody () where

import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent,
  AlonzoBbodyPredFailure (ShelleyInAlonzoBbodyPredFailure),
 )
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Ledgers ()
import Cardano.Ledger.Babel.Rules.Swaps (BabelSwapsPredFailure)
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure (LedgersFailure),
  ShelleyLedgersPredFailure,
 )

type instance EraRuleFailure "BBODY" (BabelEra c) = AlonzoBbodyPredFailure (BabelEra c)

type instance EraRuleEvent "BBODY" (BabelEra c) = AlonzoBbodyEvent (BabelEra c)

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure (BabelEra c)

instance InjectRuleFailure "BBODY" BabelSwapsPredFailure (BabelEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (BabelEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelLedgerPredFailure (BabelEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = ShelleyInAlonzoBbodyPredFailure . LedgersFailure . injectFailure

-- newtype BabelBbodyEvent era
--   = LedgersEvent (Event (EraRule "LEDGERS" era))

-- deriving stock instance
--   ( Era era
--   , Show (PredicateFailure (EraRule "LEDGERS" era))
--   ) =>
--   Show (BabelBbodyPredFailure era)

-- deriving stock instance
--   ( Era era
--   , Eq (PredicateFailure (EraRule "LEDGERS" era))
--   ) =>
--   Eq (BabelBbodyPredFailure era)

-- instance
--   ( Era era
--   , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
--   ) =>
--   NoThunks (BabelBbodyPredFailure era)

-- instance
--   ( Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
--   , Eq (PredicateFailure (EraRule "LEDGERS" era))
--   , Show (PredicateFailure (EraRule "LEDGERS" era))
--   , EraSegWits era
--   , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
--   , -- , Embed (EraRule "LEDGERS" era) (BabelBBODY era)
--     Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
--   ) =>
--   STS (BabelBBODY era)
--   where
--   type State (BabelBBODY era) = ShelleyBbodyState era

--   type Signal (BabelBBODY era) = Block (BHeaderView (EraCrypto era)) era

--   type Environment (BabelBBODY era) = BbodyEnv era

--   type BaseM (BabelBBODY era) = ShelleyBase

--   type PredicateFailure (BabelBBODY era) = BabelBbodyPredFailure era

--   type Event (BabelBBODY era) = BabelBbodyEvent era

--   initialRules = []
--   transitionRules = [bbodyTransition]

-- bbodyTransition ::
--   forall era.
--   ( Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
--   , STS (BabelBBODY era)
--   , EraSegWits era
--   , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
--   ) =>
--   TransitionRule (BabelBBODY era)
-- bbodyTransition =
--   judgmentContext
--     >>= \( TRC
--             ( BbodyEnv pp account
--               , BbodyState ls b
--               , UnserialisedBlock bhview txsSeq
--               )
--           ) -> do
--         let txs = fromTxSeq txsSeq
--             actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
--             actualBodyHash = hashTxSeq txsSeq

--         actualBodySize
--           == fromIntegral (bhviewBSize bhview)
--             ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bhview)

--         actualBodyHash
--           == bhviewBHash bhview
--             ?! InvalidBodyHashBBODY actualBodyHash (bhviewBHash bhview)

--         ls' <-
--           trans @(EraRule "LEDGERS" era) $
--             TRC (LedgersEnv (bhviewSlot bhview) pp account, ls, StrictSeq.fromStrict txs)

--         -- Note that this may not actually be a stake pool - it could be a genesis key
--         -- delegate. However, this would only entail an overhead of 7 counts, and it's
--         -- easier than differentiating here.
--         let hkAsStakePool = coerceKeyRole $ bhviewID bhview
--             slot = bhviewSlot bhview
--         firstSlotNo <- liftSTS $ do
--           ei <- asks epochInfoPure
--           e <- epochInfoEpoch ei slot
--           epochInfoFirst ei e
--         let isOverlay = isOverlaySlot firstSlotNo (pp ^. ppDG) slot
--         pure $ BbodyState ls' (incrBlocks isOverlay hkAsStakePool b)

-- instance
--   forall era ledgers.
--   ( Era era
--   , BaseM ledgers ~ ShelleyBase
--   , ledgers ~ EraRule "LEDGERS" era
--   , STS ledgers
--   , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
--   , Era era
--   ) =>
--   Embed ledgers (BabelBBODY era)
--   where
--   wrapFailed = LedgersFailure
--   wrapEvent = LedgersEvent
