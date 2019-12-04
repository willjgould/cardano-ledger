{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Utxo.QuickCheck
  ( genTx
  )
  where

import           Control.Monad (when)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Coin (Coin (..), splitCoin)
import           Generator.Core (findPayKeyPair, toAddr)
import           Generator.Core.QuickCheck (genNatural)
import           Generator.Delegation.QuickCheck (genDCerts)
import           LedgerState (pattern UTxOState)
import           MockTypes (Addr, CoreKeyPair, DCert, DPState, KeyPair, KeyPairs, Tx, TxBody, TxIn, TxOut, UTxO,
                     UTxOState, VrfKeyPairs)
import           Slot (SlotNo (..))
import           STS.Ledger (LedgerEnv (..))
import           Tx (pattern Tx, pattern TxBody, pattern TxOut)
import           Updates (emptyUpdate)
import           UTxO (pattern UTxO, balance, makeGenWitnessesVKey, makeWitnessesVKey)

import qualified Debug.Trace as D

-- | Generate a new transaction in the context of the LEDGER STS environment and state.
--
-- Selects unspent outputs and spends the funds on a some valid addresses.
-- Also generates valid certificates.
genTx :: LedgerEnv
      -> (UTxOState, DPState)
      -> KeyPairs
      -> [CoreKeyPair]
      -> VrfKeyPairs
      -> Gen Tx
genTx (LedgerEnv slot _ pparams _) (UTxOState utxo _ _ _, dpState) keys coreKeys vrfKeys = do
  keys' <- QC.shuffle keys

  -- inputs
  (witnessedInputs, spendingBalance) <- pickSpendingInputs keys' utxo
  let (inputs, spendWitnesses) = unzip witnessedInputs

  -- output addresses
  recipientAddrs <- genRecipients keys'

  ttl <- genNatural 1 100
  let slotWithTTL = slot + SlotNo (fromIntegral ttl)

  -- certificates
  (certs, certWitnesses, genesisWitnesses, deposits_, refunds_)
    <- genDCerts keys' coreKeys vrfKeys pparams dpState slot ttl

  -- attempt to make provision for certificate deposits (otherwise discard this generator)
  when (spendingBalance < deposits_)
       (D.trace ("(QC) GenTx Discarded - " <> show (spendingBalance, deposits_, refunds_)) QC.discard)
  let balance_ = spendingBalance - deposits_ + refunds_

  -- calc. fees and output amounts
  let (fee, outputs) = calcFeeAndOutputs balance_ recipientAddrs

  -- witnessed transaction
  txBody <- genTxBody (Set.fromList inputs) outputs certs fee slotWithTTL
  let !wits = makeWitnessesVKey txBody (spendWitnesses ++ certWitnesses)
              `Set.union` makeGenWitnessesVKey txBody genesisWitnesses
      multiSig = Map.empty -- TODO @uroboros Generate multi-sig transactions

  return (Tx txBody wits multiSig)

-- | Generate a transaction body with the given inputs/outputs and certificates
genTxBody
  :: Set TxIn
  -> [TxOut]
  -> Seq DCert
  -> Coin
  -> SlotNo
  -> Gen TxBody
genTxBody inputs outputs certs fee slotWithTTL = do
  return $ TxBody
             inputs
             outputs
             certs
             Map.empty -- TODO @uroboros generate withdrawals
             fee
             slotWithTTL
             emptyUpdate -- TODO @uroboros generate updates

-- | Calculate the fee and distribute the remainder of the balance
-- to the given addresses (as transaction outputs)
calcFeeAndOutputs
  :: Coin
  -> [Addr]
  -> (Coin, [TxOut])
calcFeeAndOutputs balance_ addrs =
  ( fee + splitCoinRem
  , (`TxOut` amountPerOutput) <$> addrs)
  where
    -- TODO @uroboros fee=0 works for now since PParams minFeeA/B == 0.
    --      We need to instead add a "draft" TxBody as argument, which will
    --      give a measure of the minimum fee for this transaction.
    --      This estimated fee can then be used to create the actual TxBody,
    --      with the new fee baked in.
    fee = 0
    -- split the available balance into equal portions (one for each address),
    -- if there is a remainder, then add it to the fee.
    balanceAfterFee = balance_ - fee
    (amountPerOutput, splitCoinRem) = splitCoin balanceAfterFee (fromIntegral $ length addrs)

-- | Select unspent output(s) to serve as inputs for a new transaction
--
-- Returns the inputs, paired with the KeyPair required to witness the
-- spending of the input.
-- Also returns the total spendable balance.

-- NOTE: this function needs access to the keys that the given utxo originated
-- from (in order to produce the appropriate witnesses to spend these outputs)
-- If this is not the case, findPayKeyPair will fail by not finding the matching keys.
pickSpendingInputs
  :: KeyPairs
  -> UTxO
  -> Gen ([(TxIn, KeyPair)], Coin)
pickSpendingInputs keys (UTxO utxo) = do
  selectedUtxo <- take <$> QC.choose (1, 5)
                       <*> QC.shuffle (Map.toList utxo)

  return ( witnessedInput <$> selectedUtxo
         , balance (UTxO (Map.fromList selectedUtxo)))
  where
    witnessedInput (input, TxOut addr _) = (input, findPayKeyPair addr keys)

-- | Select recipient addresses that will serve as output targets for a new transaction.
genRecipients
  :: KeyPairs
  -> Gen [Addr]
genRecipients keys = do
  let n = 1 -- TODO @uroboros select _multiple_ recipients
      recipients = take n keys

  return $ toAddr <$> recipients
