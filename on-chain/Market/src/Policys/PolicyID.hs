{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}

module Policys.PolicyID where

import qualified Plutonomy
import qualified Plutus.V1.Ledger.Address as Ledger
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import PlutusTx.Prelude

import qualified Helpers.OnChain as OnChainHelpers
import qualified Ledger.Ada as LedgerAda
import Plutus.V1.Ledger.Value (assetClass)
import qualified Plutus.V1.Ledger.Value as LedgerValue
import Types

-- Minting policy function
{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: LedgerApiV2.ValidatorHash -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkPolicyID !marketValidatorHash !redRaw !ctxRaw =
    let
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @PolicyIDRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !marketValidatorAddress = Ledger.scriptHashAddress marketValidatorHash
        ------------------
        !marketPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !marketID_AC = LedgerValue.AssetClass (marketPolicyID_CS, marketID_TN)
        ------------------
        !valueFor_Mint_Market_ID = LedgerValue.assetClassValue marketID_AC 1
    in
        ------------------
        if case redeemer of
            PolicyRedeemerMintID _ ->
                --   ---------------------
                traceIfFalse "not isMintingMarketID" isMintingMarketID
                    && traceIfFalse "not isCorrect_Output_Market_Datum" isCorrect_Output_Market_Datum
                    && traceIfFalse "not isCorrect_Output_Market_Value" isCorrect_Output_Market_Value
                    && traceIfFalse "expected zero Market inputs" (null inputs_Own_TxOuts)
                where
                    ------------------
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                    address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                   in
                                                                                                                    OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                        ]
                    ------------------
                    !outputs_Own_TxOuts =
                        [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                            address = LedgerApiV2.txOutAddress txOut
                                                                           in
                                                                            OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                        ]
                    -- ------------------
                    !output_Own_TxOut_And_Market_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC'
                        @SimpleSale
                        ctx
                        outputs_Own_TxOuts
                        marketID_AC of
                        [x] -> x
                        _ -> traceError "Expected exactly one Market output"
                    ------------------
                    !market_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum
                    ------------------
                    -- getHashDatumInput :: LedgerContextsV2.TxOut
                    -- !getHashDatumInput = case outputs_Own_TxOuts of
                    --   [o] -> o
                    --   _ -> traceError "expected exactly one input"

                    -- market_Datum_In :: SimpleSale
                    -- !market_Datum_In = case parseSimpleSale getHashDatumInput info of
                    --   Nothing -> traceError "Datum not found"
                    --   Just x -> x

                    !sellingToken_AC = assetClass (sellingToken_CS market_Datum_Out) (sellingToken_TN market_Datum_Out)

                    isMintingMarketID :: Bool
                    !isMintingMarketID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_Market_ID
                    -----------------
                    isCorrect_Output_Market_Datum :: Bool
                    !isCorrect_Output_Market_Datum =
                        let
                            !market_Datum_Out_Control =
                                SimpleSale
                                    { sellerPaymentPKH = sellerPaymentPKH market_Datum_Out
                                    , policyID_CS = marketPolicyID_CS
                                    , sellingToken_CS = sellingToken_CS market_Datum_Out
                                    , sellingToken_TN = sellingToken_TN market_Datum_Out
                                    , priceOfAsset = priceOfAsset market_Datum_Out
                                    , minADA = minADA market_Datum_Out
                                    }
                        in
                            market_Datum_Out `isEqSimpleSale` market_Datum_Out_Control
                    ----------------
                    isCorrect_Output_Market_Value :: Bool
                    !isCorrect_Output_Market_Value =
                        let
                            ---------------------
                            !policyID = LedgerValue.assetClassValue marketID_AC 1
                            --------------------
                            !sellingValue = LedgerValue.assetClassValue sellingToken_AC 1
                            ---------------------
                            !minADAFromDatum = minADA market_Datum_Out
                            !valueMinADA = LedgerAda.lovelaceValueOf minADAFromDatum
                            ---------------------
                            !valueFor_Market_Datum_Out_Control = valueMinADA <> policyID <> sellingValue
                            ---------------------
                            !valueOf_Market_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum
                        in
                            valueOf_Market_Out `OnChainHelpers.isEqValue` valueFor_Market_Datum_Out_Control
            ------------------
            PolicyRedeemerBurnID _ ->
                traceIfFalse "not isBurningMarketID" isBurningMarketID
                where
                    --   ------------------
                    !valueForBurnMarketID = LedgerValue.assetClassValue marketID_AC (negate 1)
                    ---------------------
                    isBurningMarketID :: Bool
                    isBurningMarketID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueForBurnMarketID
                    --   -----------------
            then ()
            else error ()

-- Optimized minting policy
{-# INLINEABLE policy_ID #-}
policy_ID :: LedgerApiV2.ValidatorHash -> LedgerApiV2.MintingPolicy
policy_ID marketValidatorHash = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy_ID marketValidatorHash

{-# INLINEABLE original_policy_ID #-}
original_policy_ID :: LedgerApiV2.ValidatorHash -> Plutonomy.MintingPolicy
original_policy_ID marketValidatorHash =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [||mkPolicyID||])
            `PlutusTx.applyCode` PlutusTx.liftCode marketValidatorHash

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkWrappedPolicy txHash = mkPolicyID valHash
    where
        valHash = PlutusTx.unsafeFromBuiltinData txHash :: LedgerApiV2.ValidatorHash

policyIdCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
policyIdCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicy||])
