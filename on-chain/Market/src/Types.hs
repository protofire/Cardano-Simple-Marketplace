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
data MarketRedeemer = Buy | Withdraw
PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

newtype SimpleSaleNT = SimpleSaleNT SimpleSale

{-# INLINEABLE getTypeSimpleSaleNT #-}
getTypeSimpleSaleNT :: SimpleSaleNT -> SimpleSale
getTypeSimpleSaleNT (SimpleSaleNT t) = t

{-# INLINEABLE mkTypeSimpleSale #-}
mkTypeSimpleSale ::
  LedgerApiV2.PubKeyHash ->
  CS ->
  CS ->
  TN ->
  Integer ->
  Integer ->
  SimpleSale
mkTypeSimpleSale _dSellerAddress _dPolicyID_CS _dSellingToken_CS _dSellingToken_TN _dPriceOfAsset _dMinADA =
  SimpleSale
    { sellerAddress = _dSellerAddress
    , policyID_CS = _dPolicyID_CS
    , sellingToken_CS = _dSellingToken_CS
    , sellingToken_TN = _dSellingToken_TN
    , priceOfAsset = _dPriceOfAsset
    , minADA = _dMinADA
    }

data SimpleSale = SimpleSale
  { sellerAddress :: LedgerApiV2.PubKeyHash
  , policyID_CS :: CS
  , sellingToken_CS :: CS
  , sellingToken_TN :: TN
  , priceOfAsset :: Integer
  , minADA :: Integer
  }

PlutusTx.makeIsDataIndexed
  ''SimpleSale
  [ ('SimpleSale, 0)
  ]

PlutusTx.makeIsDataIndexed
  ''SimpleSaleNT
  [ ('SimpleSaleNT, 0)
  ]

{-# INLINEABLE parseSimpleSale #-}
parseSimpleSale :: LedgerApiV2.TxOut -> LedgerApiV2.TxInfo -> Maybe SimpleSale
parseSimpleSale o info = case LedgerContextV2.txOutDatum o of
  LedgerApiV2.NoOutputDatum -> traceError "Found Collateral output but NoOutputDatum"
  LedgerApiV2.OutputDatum (LedgerApiV2.Datum d) -> PlutusTx.fromBuiltinData d
  LedgerApiV2.OutputDatumHash dh -> do
    LedgerApiV2.Datum d <- LedgerContextV2.findDatum dh info
    PlutusTx.fromBuiltinData d

{-# INLINEABLE isEqSimpleSale #-}
isEqSimpleSale :: SimpleSale -> SimpleSale -> P.Bool
isEqSimpleSale a b =
  (sellerAddress a == sellerAddress b)
    && (policyID_CS a == policyID_CS b)
    && (sellingToken_CS a == sellingToken_CS b)
    && (sellingToken_TN a == sellingToken_TN b)
    && (priceOfAsset a == priceOfAsset b)
    && (minADA a == minADA b)

{-# INLINEABLE marketID_TN #-}
marketID_TN :: LedgerApiV2.TokenName
marketID_TN = LedgerApiV2.TokenName "MarketPolicyID"
