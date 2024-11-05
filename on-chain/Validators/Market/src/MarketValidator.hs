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

module MarketValidator where

import Plutonomy qualified
import Plutus.V1.Ledger.Value qualified as LedgerValueV1
import Plutus.V2.Ledger.Api qualified as LedgerApiV2
import Plutus.V2.Ledger.Contexts qualified as LedgerContextsV2
import PlutusTx qualified

import Helpers.OnChain qualified as OnChainHelpers
import PlutusTx.Prelude (Integer, Maybe (Just, Nothing), error, traceError, traceIfFalse, ($), (&&), (==), (>=))

data MarketRedeemer = Buy | Withdraw
PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

{-# INLINEABLE mkMarket #-}
mkMarket :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkMarket datumRaw redeemerRaw ctxRaw =
    case sellerPkh of
        Nothing -> traceError "Script Address in seller"
        Just pkh -> case redeemer of
            Buy ->
                if traceIfFalse "Multiple script inputs" (OnChainHelpers.allScriptInputsCount (LedgerContextsV2.txInfoInputs info) == 1)
                    && traceIfFalse "Seller not paid" (LedgerValueV1.assetClassValueOf (LedgerContextsV2.valuePaidTo info pkh) adaAsset >= price)
                    then ()
                    else error ()
            Withdraw ->
                if traceIfFalse "Seller Signature Missing" $ LedgerContextsV2.txSignedBy info pkh
                    then ()
                    else error ()
  where
    info :: LedgerApiV2.TxInfo
    info = LedgerContextsV2.scriptContextTxInfo ctx

    ctx = PlutusTx.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    SimpleSale{sellerAddress = sellerAddr, priceOfAsset = price} = PlutusTx.unsafeFromBuiltinData @SimpleSale datumRaw
    redeemer = PlutusTx.unsafeFromBuiltinData @MarketRedeemer redeemerRaw

    sellerPkh = case sellerAddr of
        LedgerApiV2.Address cre _ -> case cre of
            LedgerApiV2.PubKeyCredential pkh -> Just pkh
            LedgerApiV2.ScriptCredential _ -> Nothing

    adaAsset = LedgerValueV1.AssetClass (LedgerApiV2.adaSymbol, LedgerApiV2.adaToken)

--------------------------------------------------------------------------------

{- | 'marketValidator' is the optimized version of the Plutus validator script.
 It uses 'Plutonomy.optimizeUPLC' to optimize the script and convert it to a Plutus V2 validator.
-}
{-# INLINEABLE marketValidator #-}
marketValidator :: LedgerApiV2.Validator
marketValidator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus plutonomyValidator

{- | 'plutonomyValidator' is the raw Plutus validator script before optimization.
 It is created using 'Plutonomy.mkValidatorScript' and compiled from 'mkMarketValidator'.
-}
{-# INLINEABLE plutonomyValidator #-}
plutonomyValidator :: Plutonomy.Validator
plutonomyValidator =
    Plutonomy.mkValidatorScript
        $$(PlutusTx.compile [||mkMarket||])

--------------------------------------------------------------------------------
