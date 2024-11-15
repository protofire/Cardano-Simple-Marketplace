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
module Helpers.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Value as LedgerValue (AssetClass)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import qualified Schema
import Prelude (Integer)

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

instance Schema.ToSchema LedgerApiV2.Validator where
  toSchema = Schema.FormSchemaUnit

-- instance Schema.ToSchema  LedgerAddress.Address where
--   toSchema = Schema.FormSchemaUnit

instance Schema.ToSchema LedgerApiV2.MintingPolicy where
  toSchema = Schema.FormSchemaUnit

-- instance Schema.ToSchema LedgerApiV2.CurrencySymbol where
--     toSchema = Schema.FormSchemaUnit

-- instance Schema.ToSchema LedgerApiV2.TokenName where
--     toSchema = Schema.FormSchemaUnit

--------------------------------------------------------------------------------2
