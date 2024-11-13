{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

--------------------------------------------------------------------------------
{- HLINT ignore "Use camelCase"               -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------
module Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Helpers.OffChain as OffChainHelpers
import qualified Plutus.V1.Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextV2
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

type CS = LedgerApiV2.CurrencySymbol
type TN = LedgerApiV2.TokenName

type NFT = LedgerValue.AssetClass
type WalletPaymentPKH = LedgerApiV2.PubKeyHash
type StakeCredentialPubKeyHash = LedgerApiV2.PubKeyHash

--------------------------------------------------------------------------------2

-- instance Schema.ToSchema LedgerApiV2.Validator where
--   toSchema = Schema.FormSchemaUnit
--
-- -- TODO: para cuando vuelva a usar plutus-1.1.0, tengo que desactivar esto
-- -- instance Schema.ToSchema  LedgerAddress.Address where
-- --   toSchema = Schema.FormSchemaUnit
--
-- instance Schema.ToSchema LedgerApiV2.MintingPolicy where
--   toSchema = Schema.FormSchemaUnit
--
-- -- instance Schema.ToSchema LedgerApiV2.CurrencySymbol where
-- --     toSchema = Schema.FormSchemaUnit
--
-- -- instance Schema.ToSchema LedgerApiV2.TokenName where
-- --     toSchema = Schema.FormSchemaUnit

--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
    deriving (P.Show, P.Eq)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintIDType == PolicyRedeemerMintIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
    deriving (P.Show, P.Eq)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerBurnIDType == PolicyRedeemerBurnIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerBurnIDType

data PolicyIDRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (P.Show, P.Eq)

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

--------------------------------------------------------------------------------

data MarketRedeemer = Buy | Withdraw
    deriving (P.Show, P.Eq)

PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

--------------------------------------------------------------------------------2

data SimpleSale = SimpleSale
    { sellerPaymentPKH :: LedgerApiV2.PubKeyHash
    , policyID_CS :: CS
    , sellingToken_CS :: CS
    , sellingToken_TN :: TN
    , priceOfAsset :: Integer
    , minADA :: Integer
    }
    deriving (P.Show, P.Eq)

PlutusTx.makeIsDataIndexed
    ''SimpleSale
    [ ('SimpleSale, 0)
    ]

{-# INLINEABLE mkTypeSimpleSale #-}
mkTypeSimpleSale ::
    LedgerApiV2.PubKeyHash ->
    CS ->
    CS ->
    TN ->
    Integer ->
    Integer ->
    SimpleSale
mkTypeSimpleSale _dSellerPaymentPKH _dPolicyID_CS _dSellingToken_CS _dSellingToken_TN _dPriceOfAsset _dMinADA =
    SimpleSale
        { sellerPaymentPKH = _dSellerPaymentPKH
        , policyID_CS = _dPolicyID_CS
        , sellingToken_CS = _dSellingToken_CS
        , sellingToken_TN = _dSellingToken_TN
        , priceOfAsset = _dPriceOfAsset
        , minADA = _dMinADA
        }

--------------------------------------------------------------------------------

{-# INLINEABLE parseSimpleSale #-}
parseSimpleSale :: LedgerApiV2.TxOut -> LedgerApiV2.TxInfo -> Maybe SimpleSale
parseSimpleSale o info = case LedgerContextV2.txOutDatum o of
    LedgerApiV2.NoOutputDatum -> traceError "Found output but NoOutputDatum"
    LedgerApiV2.OutputDatum (LedgerApiV2.Datum d) -> PlutusTx.fromBuiltinData d
    LedgerApiV2.OutputDatumHash dh -> do
        LedgerApiV2.Datum d <- LedgerContextV2.findDatum dh info
        PlutusTx.fromBuiltinData d

{-# INLINEABLE isEqSimpleSale #-}
isEqSimpleSale :: SimpleSale -> SimpleSale -> P.Bool
isEqSimpleSale a b =
    (sellerPaymentPKH a == sellerPaymentPKH b)
        && (policyID_CS a == policyID_CS b)
        && (sellingToken_CS a == sellingToken_CS b)
        && (sellingToken_TN a == sellingToken_TN b)
        && (priceOfAsset a == priceOfAsset b)
        && (minADA a == minADA b)

--------------------------------------------------------------------------------

{-# INLINEABLE marketID_TN #-}
marketID_TN :: LedgerApiV2.TokenName
marketID_TN = LedgerApiV2.TokenName "MarketPolicyID"

--------------------------------------------------------------------------------

-- readStringDecodedAsPolicyIDRedeemer "{\"getRedeemer\":\"d87a9fd87980ff\"}"

readStringDecodedAsPolicyIDRedeemer :: P.String -> P.IO PolicyIDRedeemer
readStringDecodedAsPolicyIDRedeemer encoded = do
    raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let
        result = LedgerApiV2.unsafeFromBuiltinData @PolicyIDRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsPolicyNFTRedeemer "{\"getRedeemer\":\"d87a9fd87980ff\"}"

readStringDecodedAsPolicyNFTRedeemer :: P.String -> P.IO MarketRedeemer
readStringDecodedAsPolicyNFTRedeemer encoded = do
    raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let
        result = LedgerApiV2.unsafeFromBuiltinData @MarketRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsMarketValidatorDatum "{\"getDatum\":\"d8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e581c97646926632d88c2e18370b48b79929b733e51709b8cbdce65690b02581c018d8b7fd7241e65b515e2ece48fa453fd14f7e6319ebefbdcd3c6374443494458182c1a001a3f39ff\"}"
readStringDecodedAsMarketValidatorDatum :: P.String -> P.IO SimpleSale
readStringDecodedAsMarketValidatorDatum encoded = do
    raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let
        result = LedgerApiV2.unsafeFromBuiltinData @SimpleSale (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result
