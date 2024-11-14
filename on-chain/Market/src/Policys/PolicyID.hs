
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

-- Import required modules
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

-- Minting policy function definition
{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: LedgerApiV2.ValidatorHash -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkPolicyID !marketValidatorHash !redRaw !ctxRaw =
    let
        -- Convert redeemer and context from BuiltinData to their types
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @PolicyIDRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        
        -- Define market validator address and asset class for market ID
        !marketValidatorAddress = Ledger.scriptHashAddress marketValidatorHash
        !marketPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !marketID_AC = LedgerValue.AssetClass (marketPolicyID_CS, marketID_TN)

        -- Value to mint for market ID
        !valueFor_Mint_Market_ID = LedgerValue.assetClassValue marketID_AC 1
    in
        -- Check conditions based on the redeemer
        if case redeemer of
            PolicyRedeemerMintID _ ->
                -- Check for minting market ID and correct output datum and value
                traceIfFalse "not isMintingMarketID" isMintingMarketID
                    && traceIfFalse "not isCorrect_Output_Market_Datum" isCorrect_Output_Market_Datum
                    && traceIfFalse "not isCorrect_Output_Market_Value" isCorrect_Output_Market_Value
                    && traceIfFalse "expected zero Market inputs" (null inputs_Own_TxOuts)
                where
                    -- Inputs and outputs associated with this validator
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                    address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                   in
                                                                                                                    OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                        ]

                    !outputs_Own_TxOuts =
                        [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                            address = LedgerApiV2.txOutAddress txOut
                                                                           in
                                                                            OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                        ]

                    -- Extract datum from output
                    !output_Own_TxOut_And_Market_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC'
                        @SimpleSale
                        ctx
                        outputs_Own_TxOuts
                        marketID_AC of
                        [x] -> x
                        _ -> traceError "Expected exactly one Market output"

                    -- Get the market datum from the output
                    !market_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum

                    -- Asset class for selling token
                    !sellingToken_AC = assetClass (sellingToken_CS market_Datum_Out) (sellingToken_TN market_Datum_Out)

                    -- Check if market ID is being minted
                    isMintingMarketID :: Bool
                    !isMintingMarketID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_Market_ID

                    -- Check if the market datum is correct
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

                    -- Check if the market value is correct
                    isCorrect_Output_Market_Value :: Bool
                    !isCorrect_Output_Market_Value =
                        let
                            !policyID = LedgerValue.assetClassValue marketID_AC 1
                            !sellingValue = LedgerValue.assetClassValue sellingToken_AC 1
                            !minADAFromDatum = minADA market_Datum_Out
                            !valueMinADA = LedgerAda.lovelaceValueOf minADAFromDatum
                            !valueFor_Market_Datum_Out_Control = valueMinADA <> policyID <> sellingValue
                            !valueOf_Market_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum
                        in
                            valueOf_Market_Out `OnChainHelpers.isEqValue` valueFor_Market_Datum_Out_Control

            PolicyRedeemerBurnID _ ->
                traceIfFalse "not isBurningMarketID" isBurningMarketID
                where
                    -- Value to burn for market ID
                    !valueForBurnMarketID = LedgerValue.assetClassValue marketID_AC (negate 1)

                    -- Check if market ID is being burned
                    isBurningMarketID :: Bool
                    isBurningMarketID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueForBurnMarketID
            then ()
            else error ()

-- Optimized minting policy to return a minting policy script
{-# INLINEABLE policy_ID #-}
policy_ID :: LedgerApiV2.ValidatorHash -> LedgerApiV2.MintingPolicy
policy_ID marketValidatorHash = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy_ID marketValidatorHash

-- Create the original minting policy script
{-# INLINEABLE original_policy_ID #-}
original_policy_ID :: LedgerApiV2.ValidatorHash -> Plutonomy.MintingPolicy
original_policy_ID marketValidatorHash =
    Plutonomy.mkMintingPolicyScript $
        $$(PlutusTx.compile [||mkPolicyID||])
            `PlutusTx.applyCode` PlutusTx.liftCode marketValidatorHash

-- Wrapper for policy
{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkWrappedPolicy txHash = mkPolicyID valHash
    where
        valHash = PlutusTx.unsafeFromBuiltinData txHash :: LedgerApiV2.ValidatorHash

-- Compile the final policy code
policyIdCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
policyIdCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicy||])
