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
import qualified Plutus.V1.Ledger.Value as LedgerValue
import Types (SimpleSale (sellingToken))
import qualified Types as T

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintIDType == PolicyRedeemerMintIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerBurnIDType == PolicyRedeemerBurnIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerBurnIDType

data PolicyIDRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

instance Eq PolicyIDRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''PolicyIDRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]

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
        !marketID_AC = LedgerValue.AssetClass (marketPolicyID_CS, T.marketID_TN)
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
                    [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info, let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                in OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                    ]
                ------------------
                !outputs_Own_TxOuts =
                    [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, let address = LedgerApiV2.txOutAddress txOut
                                                                        in OnChainHelpers.isScriptAddress address && address == marketValidatorAddress
                    ]
                -- ------------------
                -- !output_Own_TxOut_And_Market_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                --     @T.SimpleSaleNT @T.SimpleSale
                --     ctx
                --     outputs_Own_TxOuts
                --     marketID_AC
                --     T.getTypeMarketNT of
                --     [x] -> x
                --     _ -> traceError "Expected exactly one Market output"
                -- ------------------
                -- !market_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum
                ------------------
                getHashDatumInput :: LedgerContextsV2.TxOut
                !getHashDatumInput = case outputs_Own_TxOuts of
                    [o] -> o
                    _ -> traceError "expected exactly one input"

                market_Datum_Out :: T.SimpleSale
                !market_Datum_Out = case T.parseSimpleSale getHashDatumInput info of
                    Nothing -> traceError "Datum not found"
                    Just x -> x

                isMintingMarketID :: Bool
                !isMintingMarketID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_Market_ID
                -----------------
                isCorrect_Output_Market_Datum :: Bool
                !isCorrect_Output_Market_Datum =
                    let !market_Datum_Out_Control =
                            T.SimpleSale{sellerAddress = T.sellerAddress market_Datum_Out, policyID = T.policyID market_Datum_Out, sellingToken = T.sellingToken market_Datum_Out, priceOfAsset = T.priceOfAsset market_Datum_Out}
                     in market_Datum_Out `T.isEqSimpleSale` market_Datum_Out_Control
                ----------------
                isCorrect_Output_Market_Value :: Bool
                !isCorrect_Output_Market_Value =
                    let
                        ---------------------
                        !policyID = LedgerValue.assetClassValue marketID_AC 1
                        ---------------------
                        !sellingValue = LedgerValue.assetClassValue (T.sellingToken market_Datum_Out) 1
                        ---------------------
                        !valueFor_Market_Datum_Out_Control = policyID <> sellingValue ---------------------
                        !valueOf_Market_Out = LedgerApiV2.txOutValue getHashDatumInput -- OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Market_Datum
                     in
                        valueOf_Market_Out `OnChainHelpers.isEqValue` valueFor_Market_Datum_Out_Control
            ------------------
            PolicyRedeemerBurnID _ -> True
              where
                --   traceIfFalse "not isBurningMarketID" isBurningMarketID

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
