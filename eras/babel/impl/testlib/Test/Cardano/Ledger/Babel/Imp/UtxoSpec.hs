{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Babel.Imp.UtxoSpec -- (spec)
where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage.TxBody (referenceInputsTxBodyL)
import Cardano.Ledger.Babbage.TxOut (referenceScriptTxOutL)
import Cardano.Ledger.Babel.Era
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.MemoBytes (getMemoRawBytes)
import Cardano.Ledger.Plutus.Language (SLanguage (..), hashPlutusScript, plutusBinary)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.UTxO (getShelleyMinFeeTxUtxo)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (getMinFeeTxUtxo)
import Cardano.Ledger.Val
import qualified Data.ByteString.Short as SBS (length)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babel.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkScriptAddr)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceeds3)

spec ::
  SpecWith (ImpTestState (BabelEra StandardCrypto))
spec = describe "UTxO" $ do
  describe "Reference scripts" $ do
    it "required reference script counts towards the minFee calculation" $ do
      spendingScript <- nativeScript
      checkMinFee spendingScript [fromNativeScript spendingScript]

    it "reference scripts not required for spending the input count towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFee spendingScript $
        fromNativeScript spendingScript : extraScripts

    it "a scripts referenced several times counts for each reference towards the minFee calculation" $ do
      spendingScript <- nativeScript
      extraScripts <- distinctScripts
      checkMinFee spendingScript $
        [fromNativeScript spendingScript, fromNativeScript spendingScript]
          ++ extraScripts
          ++ extraScripts
  where
    checkMinFee ::
      NativeScript (BabelEra StandardCrypto) ->
      [Script (BabelEra StandardCrypto)] ->
      ImpTestM (BabelEra StandardCrypto) ()
    checkMinFee scriptToSpend refScripts = do
      refScriptFee <- setRefScriptFee
      logEntry "lock an input with a script"
      scriptSpendIn <- createScriptUtxo scriptToSpend
      logEntry
        "create outputs with reference scripts and the return them mapped to their corresponding inputs"
      refScriptInToScripts <- createRefScriptsUtxos refScripts
      logEntry "spend the initial input by passing the reference scripts"
      tx <- spendScriptUsingRefScripts scriptSpendIn $ Map.keysSet refScriptInToScripts
      logEntry
        "compute the difference between the current-(BabelEra StandardCrypto) minFee and that computed in pre-Babel eras"
      minFeeDiff <- conwayDiffMinFee tx
      logEntry "check that the difference is the sum of the sizes of the passed reference scripts"
      minFeeDiff
        `shouldBe` Coin
          ( floor $
              fromIntegral @Int @Rational (sum $ scriptSize <$> refScriptInToScripts)
                * unboundRational refScriptFee
          )

    distinctScripts :: ImpTestM (BabelEra StandardCrypto) [Script (BabelEra StandardCrypto)]
    distinctScripts = do
      nativeScripts <-
        (fromNativeScript <$>)
          <$> replicateM 3 nativeScript
      let
        -- TODO WG
        psh1 = hashPlutusScript $ alwaysSucceeds3 SPlutusV4
      ps1 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScriptMaybe psh1
      let
        psh2 = hashPlutusScript $ alwaysSucceeds3 SPlutusV4
      ps2 <- impAnn "Expecting Plutus script" . expectJust $ impLookupPlutusScriptMaybe psh2
      let plutusScripts = [fromPlutusScript ps1, fromPlutusScript ps2]
      pure $ nativeScripts ++ plutusScripts

    conwayDiffMinFee :: Tx (BabelEra StandardCrypto) -> ImpTestM (BabelEra StandardCrypto) Coin
    conwayDiffMinFee tx = do
      utxo <- getUTxO
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      pure $ getMinFeeTxUtxo pp tx utxo <-> getShelleyMinFeeTxUtxo pp tx

    createScriptUtxo ::
      NativeScript (BabelEra StandardCrypto) ->
      ImpTestM (BabelEra StandardCrypto) (TxIn (EraCrypto (BabelEra StandardCrypto)))
    createScriptUtxo script = do
      scriptAddr <- addScriptAddr script
      tx <-
        submitTx . mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL @(BabelEra StandardCrypto)
            .~ SSeq.fromList [mkBasicTxOut @(BabelEra StandardCrypto) scriptAddr (inject (Coin 1000))]
      pure $ txInAt (0 :: Int) tx

    createRefScriptsUtxos ::
      [Script (BabelEra StandardCrypto)] ->
      ImpTestM
        (BabelEra StandardCrypto)
        (Map.Map (TxIn (EraCrypto (BabelEra StandardCrypto))) (Script (BabelEra StandardCrypto)))
    createRefScriptsUtxos scripts = do
      rootOut <- snd <$> lookupImpRootTxOut
      let outs =
            scripts
              <&> ( \s ->
                      mkBasicTxOut @(BabelEra StandardCrypto) (rootOut ^. addrTxOutL) (inject (Coin 100))
                        & referenceScriptTxOutL @(BabelEra StandardCrypto)
                        .~ SJust s
                  )
      tx <-
        submitTx . mkBasicTx $
          mkBasicTxBody
            & outputsTxBodyL @(BabelEra StandardCrypto)
            .~ SSeq.fromList outs
      let refIns = (`txInAt` tx) <$> [0 .. length scripts - 1]
      pure $ Map.fromList $ refIns `zip` scripts

    spendScriptUsingRefScripts ::
      TxIn (EraCrypto (BabelEra StandardCrypto)) ->
      Set.Set (TxIn (EraCrypto (BabelEra StandardCrypto))) ->
      ImpTestM (BabelEra StandardCrypto) (Tx (BabelEra StandardCrypto))
    spendScriptUsingRefScripts scriptIn refIns =
      submitTxAnn "spendScriptUsingRefScripts" . mkBasicTx $
        mkBasicTxBody
          & inputsTxBodyL @(BabelEra StandardCrypto)
          .~ Set.singleton scriptIn
          & referenceInputsTxBodyL @(BabelEra StandardCrypto)
          .~ refIns

    nativeScript :: ImpTestM (BabelEra StandardCrypto) (NativeScript (BabelEra StandardCrypto))
    nativeScript = do
      requiredKeyHash <- freshKeyHash
      let script = RequireAllOf (SSeq.singleton (RequireSignature @(BabelEra StandardCrypto) requiredKeyHash))
      _ <- impAddNativeScript script
      pure script

    addScriptAddr ::
      NativeScript (BabelEra StandardCrypto) ->
      ImpTestM (BabelEra StandardCrypto) (Addr (EraCrypto (BabelEra StandardCrypto)))
    addScriptAddr script = do
      kpStaking1 <- lookupKeyPair =<< freshKeyHash
      scriptHash <- impAddNativeScript script
      pure $ mkScriptAddr scriptHash kpStaking1

    scriptSize :: Script (BabelEra StandardCrypto) -> Int
    scriptSize = \case
      TimelockScript tl -> SBS.length $ getMemoRawBytes tl
      PlutusScript ps -> withPlutusScript ps (SBS.length . unPlutusBinary . plutusBinary)

    setRefScriptFee :: ImpTestM (BabelEra StandardCrypto) NonNegativeInterval
    setRefScriptFee = do
      let refScriptFee = 10 %! 1
      modifyPParams $ ppMinFeeRefScriptCostPerByteL .~ refScriptFee
      pure refScriptFee
