{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns#-}

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
import PlutusTx.Prelude hiding (unless)
import Types
    ( SimpleSale(policyID, sellingToken, priceOfAsset, sellerAddress),
      SimpleSaleNT,
      getTypeSimpleSaleNT ) 

data MarketRedeemer = Buy | Withdraw
PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

{-# INLINEABLE mkMarket #-}
mkMarket :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMarket datumRaw redeemerRaw ctxRaw =
------------------------------- Resources --------------------------------------
    let !redeemer = PlutusTx.unsafeFromBuiltinData @MarketRedeemer redeemerRaw
        !ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        !datum_In = PlutusTx.unsafeFromBuiltinData @SimpleSale datumRaw

        !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !callOptionValidatorAddress = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
        ------------------
        !policyID_AC = policyID datum_In 

        !inputs_Own_TxOuts =
            [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info,
            let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
            in OnChainHelpers.isScriptAddress address && address == callOptionValidatorAddress
            ]
        !outputs_Own_TxOuts =
            [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, let address = LedgerApiV2.txOutAddress txOut
                                                                in OnChainHelpers.isScriptAddress address && address == callOptionValidatorAddress
            ]
        !output_Own_TxOut_And_SimpleSale_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
            @SimpleSaleNT
            @SimpleSale
            ctx
            outputs_Own_TxOuts
            policyID_AC
            getTypeSimpleSaleNT of
            [x] -> x
            _ -> traceError "Expected exactly one SimpleSale output"
        !callOption_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SimpleSale_Datum
--------------------------------- Buy ------------------------------------------
        !validateBuyNFTSimpleSale =
            traceIfFalse "Premium not paid" premiumPaid
                && traceIfFalse "Multiple script inputs" (OnChainHelpers.allScriptInputsCount (LedgerContextsV2.txInfoInputs info) == 1)
                && traceIfFalse "not isCorrect_Output_SimpleSale_Datum" isCorrect_Output_SimpleSale_Datum
                && traceIfFalse "not isCorrect_Output_SimpleSale_Value" isCorrect_Output_SimpleSale_Value
          where
            isCorrect_Output_SimpleSale_Datum :: Bool
            !isCorrect_Output_SimpleSale_Datum =
                let !callOption_Datum_Out_Control = datum_In
                in callOption_Datum_Out `OnChainHelpers.isUnsafeEqDatums` callOption_Datum_Out_Control
            ------------------
            isCorrect_Output_SimpleSale_Value :: Bool
            !isCorrect_Output_SimpleSale_Value =
                let
                    ---------------------
                    !policyIDToken = LedgerValue.assetClassValue policyID_AC 1
                    ---------------------
                    !sellingValue = LedgerValue.assetClassValue (sellingToken callOption_Datum_Out) 1
                    ---------------------
                    !valueFor_SimpleSale_Datum_Out_Control = policyIDToken <> sellingValue
                    ---------------------
                    !valueOf_SimpleSale_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SimpleSale_Datum
                 in
                    valueOf_SimpleSale_Out `OnChainHelpers.isEqValue` valueFor_SimpleSale_Datum_Out_Control

            premiumPaid :: Bool
            !premiumPaid = LedgerContextsV2.valuePaidTo info (sellerAddress datum_In) == LedgerApiV2.singleton LedgerApiV2.adaSymbol LedgerApiV2.adaToken (priceOfAsset datum_In)
--------------------------------- Withdraw -------------------------------------
        !validateSellerGetBack =
            traceIfFalse "Owner not signed" signedBySeller
          where

            signedBySeller :: Bool
            !signedBySeller = LedgerContextsV2.txSignedBy info $ sellerAddress datum_In
                  
------------------------------- Conditions -------------------------------------
    in if traceIfFalse "Expected exactly one SellOffer input" (length inputs_Own_TxOuts == 1)
      && (case redeemer of
            Buy -> validateBuyNFTSimpleSale
            Withdraw -> validateSellerGetBack
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
