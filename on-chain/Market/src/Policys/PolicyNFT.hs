{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

{- HLINT ignore "Use camelCase" -}

module Policys.PolicyNFT where

import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

-- Data type for redeemer
data NFTRedeemer = Mint | Burn
PlutusTx.unstableMakeIsData ''NFTRedeemer

-- Minting policy function
{-# INLINABLE mkPolicyNFT #-}
mkPolicyNFT :: LedgerApiV2.TxOutRef -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkPolicyNFT oref redRaw ctxRaw =
    if case redeemer of
        Burn -> traceIfFalse "Wrong amount burned" checkBurnAmount
        Mint -> traceIfFalse "UTxO not consumed"   hasInputUTxO &&
                traceIfFalse "Wrong amount minted" checkMintedAmount
    then () else error ()
    where
        ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        redeemer = LedgerApiV2.unsafeFromBuiltinData @NFTRedeemer redRaw
        info = LedgerContextsV2.scriptContextTxInfo ctx
        hasInputUTxO = any (\i -> LedgerApiV2.txInInfoOutRef i == oref) $ LedgerApiV2.txInfoInputs info
        cs = LedgerContextsV2.ownCurrencySymbol ctx
        checkMintedAmount = allOnes (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))
        checkBurnAmount = allOnesNegatives (LedgerValue.flattenValue (LedgerApiV2.txInfoMint info))

        allOnes lst = all (\(cs', _, amt) -> cs' == cs && amt == 1) lst
        allOnesNegatives lst = all (\(cs', _, amt) -> cs' == cs && amt == (-1)) lst

-- Optimized minting policy
{-# INLINEABLE policy_NFT #-}
policy_NFT :: LedgerApiV2.TxOutRef -> LedgerApiV2.MintingPolicy
policy_NFT oref = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ original_policy_NFT oref

{-# INLINEABLE original_policy_NFT #-}
original_policy_NFT :: LedgerApiV2.TxOutRef -> Plutonomy.MintingPolicy
original_policy_NFT oref =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkPolicyNFT ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
----------------------------------------------------------------------------------------

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkWrappedPolicy txHash txOutputIndex = mkPolicyNFT txOutRef
    where
        tid = PlutusTx.unsafeFromBuiltinData txHash :: BuiltinByteString
        txOutRef =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid
                , LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData txOutputIndex
                }

nftPolicyCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
nftPolicyCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicy||])

-------------------------------------------------------------------------------------------------------------------------------------------------------
