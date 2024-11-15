{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Module Overview
-- This module defines various types and utilities for a marketplace smart contract
-- implementation on Cardano. It includes data structures for representing sales,
-- redeemers for minting/burning policies, and utility functions for encoding/decoding.
--------------------------------------------------------------------------------
module Types where

--------------------------------------------------------------------------------
-- External Imports
--------------------------------------------------------------------------------

-- Import helper functions, Plutus-specific libraries, and standard Haskell libraries.
import qualified Helpers.OffChain as OffChainHelpers
import qualified Plutus.V1.Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextV2
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless) -- Import Plutus Prelude, excluding specific functions.
import qualified Prelude as P

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- Define type aliases for readability and reusability throughout the module.
type CS = LedgerApiV2.CurrencySymbol
type TN = LedgerApiV2.TokenName
type NFT = LedgerValue.AssetClass
type WalletPaymentPKH = LedgerApiV2.PubKeyHash
type StakeCredentialPubKeyHash = LedgerApiV2.PubKeyHash

--------------------------------------------------------------------------------
-- Redeemer Types for Minting and Burning Policies
--------------------------------------------------------------------------------

-- Define redeemer types for minting and burning tokens.
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
    deriving (P.Show, P.Eq)

-- Custom equality implementation for mint ID redeemer.
instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintIDType == PolicyRedeemerMintIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
    deriving (P.Show, P.Eq)

-- Custom equality implementation for burn ID redeemer.
instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerBurnIDType == PolicyRedeemerBurnIDType = True

PlutusTx.unstableMakeIsData ''PolicyRedeemerBurnIDType

-- Combined redeemer type for both minting and burning policies.
data PolicyIDRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (P.Show, P.Eq)

-- Custom equality implementation for the combined redeemer type.
instance Eq PolicyIDRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _ = False

-- Associate the redeemer constructors with indices for serialization.
PlutusTx.makeIsDataIndexed
    ''PolicyIDRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]

--------------------------------------------------------------------------------
-- Redeemer Types for Market Actions
--------------------------------------------------------------------------------

-- Define redeemer for market-specific actions (e.g., Buy, Withdraw).
data MarketRedeemer = Buy | Withdraw
    deriving (P.Show, P.Eq)

-- Associate market redeemer constructors with indices for serialization.
PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

--------------------------------------------------------------------------------
-- Marketplace Sale Data Structure
--------------------------------------------------------------------------------

-- Marketplace version constant.
marketPlaceVersion :: Integer
marketPlaceVersion = 1

-- Define the data structure for a simple sale in the marketplace.
data SimpleSale = SimpleSale
    { version :: Integer
    , sellerPaymentPKH :: LedgerApiV2.PubKeyHash
    , policyID_CS :: CS
    , sellingToken_CS :: CS
    , sellingToken_TN :: TN
    , priceOfAsset :: Integer
    , minADA :: Integer
    }
    deriving (P.Show, P.Eq)

-- Associate the SimpleSale constructor with an index for serialization.
PlutusTx.makeIsDataIndexed
    ''SimpleSale
    [ ('SimpleSale, 0)
    ]

-- Inlineable function to create a SimpleSale instance.
{-# INLINEABLE mkTypeSimpleSale #-}
mkTypeSimpleSale ::
    LedgerApiV2.PubKeyHash -> CS -> CS -> TN -> Integer -> Integer -> SimpleSale
mkTypeSimpleSale _dSellerPaymentPKH _dPolicyID_CS _dSellingToken_CS _dSellingToken_TN _dPriceOfAsset _dMinADA =
    SimpleSale
        { version = marketPlaceVersion
        , sellerPaymentPKH = _dSellerPaymentPKH
        , policyID_CS = _dPolicyID_CS
        , sellingToken_CS = _dSellingToken_CS
        , sellingToken_TN = _dSellingToken_TN
        , priceOfAsset = _dPriceOfAsset
        , minADA = _dMinADA
        }

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- Parse a SimpleSale from a transaction output's datum.
{-# INLINEABLE parseSimpleSale #-}
parseSimpleSale :: LedgerApiV2.TxOut -> LedgerApiV2.TxInfo -> Maybe SimpleSale
parseSimpleSale o info = case LedgerContextV2.txOutDatum o of
    LedgerApiV2.NoOutputDatum -> traceError "Found output but NoOutputDatum"
    LedgerApiV2.OutputDatum (LedgerApiV2.Datum d) -> PlutusTx.fromBuiltinData d
    LedgerApiV2.OutputDatumHash dh -> do
        LedgerApiV2.Datum d <- LedgerContextV2.findDatum dh info
        PlutusTx.fromBuiltinData d

-- Check if two SimpleSale instances are equal.
{-# INLINEABLE isEqSimpleSale #-}
isEqSimpleSale :: SimpleSale -> SimpleSale -> P.Bool
isEqSimpleSale a b =
    (version a == version b)
        && (sellerPaymentPKH a == sellerPaymentPKH b)
        && (policyID_CS a == policyID_CS b)
        && (sellingToken_CS a == sellingToken_CS b)
        && (sellingToken_TN a == sellingToken_TN b)
        && (priceOfAsset a == priceOfAsset b)
        && (minADA a == minADA b)

--------------------------------------------------------------------------------
-- Hardcoded Token Name for Market Policy
--------------------------------------------------------------------------------

{-# INLINEABLE marketID_TN #-}
marketID_TN :: LedgerApiV2.TokenName
marketID_TN = LedgerApiV2.TokenName "MarketPolicyID"

--------------------------------------------------------------------------------
-- JSON Decoding Utilities
--------------------------------------------------------------------------------

-- Decode a PolicyIDRedeemer from a JSON string.
readStringDecodedAsPolicyIDRedeemer :: P.String -> P.IO PolicyIDRedeemer
readStringDecodedAsPolicyIDRedeemer encoded = do
    raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let result = LedgerApiV2.unsafeFromBuiltinData @PolicyIDRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- Decode a MarketRedeemer from a JSON string.
readStringDecodedAsPolicyNFTRedeemer :: P.String -> P.IO MarketRedeemer
readStringDecodedAsPolicyNFTRedeemer encoded = do
    raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let result = LedgerApiV2.unsafeFromBuiltinData @MarketRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- Decode a SimpleSale datum from a JSON string.
readStringDecodedAsMarketValidatorDatum :: P.String -> P.IO SimpleSale
readStringDecodedAsMarketValidatorDatum encoded = do
    raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let result = LedgerApiV2.unsafeFromBuiltinData @SimpleSale (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

