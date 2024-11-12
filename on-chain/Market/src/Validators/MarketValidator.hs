{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Validators.MarketValidator where

import Plutonomy qualified
import Plutus.V1.Ledger.Value qualified as LedgerValue
import Plutus.V2.Ledger.Api qualified as LedgerApiV2
import Plutus.V2.Ledger.Contexts qualified as LedgerContextsV2
import PlutusTx qualified

import Helpers.OnChain qualified as OnChainHelpers
import Ledger.Ada qualified as LedgerAda
import Plutus.V1.Ledger.Value (assetClass)
import PlutusTx.Prelude hiding (unless)
import Types

{-# INLINEABLE mkMarket #-}
mkMarket :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMarket datumRaw redeemerRaw ctxRaw =
    ------------------------------- Resources --------------------------------------
    let
        !redeemer = PlutusTx.unsafeFromBuiltinData @MarketRedeemer redeemerRaw
        !ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        !datum_In = PlutusTx.unsafeFromBuiltinData @SimpleSale datumRaw
        ------------------
        !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !marketValidatorAddress = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
        ------------------
        !policyID_AC = assetClass (policyID_CS datum_In) marketID_TN
        !sellingToken_AC = assetClass (sellingToken_CS datum_In) (sellingToken_TN datum_In)

        !inputsOwnTxOuts =
            [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                        address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                       in
                                                                                                        OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
            ]
        !outputsOwnTxOuts =
            [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                address = LedgerApiV2.txOutAddress txOut
                                                               in
                                                                OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
            ]

        !input_Own_TxOut_And_SimpleSale_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC'
            @SimpleSale
            ctx
            inputsOwnTxOuts
            policyID_AC of
            [x] -> x
            _ -> traceError "Expected exactly one SimpleSale input"

        !callOption_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum input_Own_TxOut_And_SimpleSale_Datum
        --------------------------------- Buy ------------------------------------------
        validateBuyNFTSimpleSale =
            traceIfFalse "Seller not paid" sellerPaid
                && traceIfFalse "Expected one input" (length inputsOwnTxOuts == 1)
                && traceIfFalse "Expected no outputs" (null outputsOwnTxOuts)
                && traceIfFalse "not isCorrect_Output_SimpleSale_Datum" isCorrect_Input_SimpleSale_Datum
                && traceIfFalse "not isCorrect_Output_SimpleSale_Value" isCorrect_Input_SimpleSale_Value
            where
                isCorrect_Input_SimpleSale_Datum :: Bool
                !isCorrect_Input_SimpleSale_Datum =
                    let
                        !callOption_Datum_Out_Control = datum_In
                    in
                        callOption_Datum_Out `isEqSimpleSale` callOption_Datum_Out_Control
                ------------------
                isCorrect_Input_SimpleSale_Value :: Bool
                !isCorrect_Input_SimpleSale_Value =
                    let
                        ---------------------
                        !policyIDToken = LedgerValue.assetClassValue policyID_AC 1
                        ---------------------
                        !sellingValue = LedgerValue.assetClassValue sellingToken_AC 1
                        ---------------------
                        !minADAFromDatum = minADA datum_In
                        !valueMinADA = LedgerAda.lovelaceValueOf minADAFromDatum
                        ---------------------
                        !valueFor_SimpleSale_Datum_Out_Control = valueMinADA <> policyIDToken <> sellingValue
                        ---------------------
                        !valueOf_SimpleSale_Out = OnChainHelpers.getValue_In_TxOut_And_Datum input_Own_TxOut_And_SimpleSale_Datum
                    in
                        valueOf_SimpleSale_Out `OnChainHelpers.isEqValue` valueFor_SimpleSale_Datum_Out_Control
                sellerPaid :: Bool
                !sellerPaid = OnChainHelpers.isIncludeValue (LedgerContextsV2.valuePaidTo info (sellerPaymentPKH datum_In)) (LedgerApiV2.singleton LedgerApiV2.adaSymbol LedgerApiV2.adaToken (priceOfAsset datum_In + minADA datum_In))

        --------------------------------- Withdraw -------------------------------------
        validateSellerWithdraw =
            traceIfFalse "Owner not signed" signedBySeller
            where
                signedBySeller :: Bool
                !signedBySeller = LedgerContextsV2.txSignedBy info $ sellerPaymentPKH datum_In
    in
        ------------------------------- Conditions -------------------------------------
        if traceIfFalse "Expected exactly one SellOffer input" (length inputsOwnTxOuts == 1)
            && ( case redeemer of
                    Buy -> validateBuyNFTSimpleSale
                    Withdraw -> validateSellerWithdraw
               )
            then ()
            else error ()

{- | 'marketValidator' is the optimized version of the Plutus validator script.
 It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
-}
{-# INLINEABLE marketValidator #-}
marketValidator :: LedgerApiV2.Validator
marketValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator

{- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
 It is created using 'Plutonomy.mkMarketScript' and compiled from 'mkMarketValidator'.
-}
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript
        $$(PlutusTx.compile [||mkMarket||])

--------------------------------------------------------------------------------
