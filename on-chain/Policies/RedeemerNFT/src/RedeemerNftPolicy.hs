{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module NFTCallOptions.Policys.PolicyRights where

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified Plutus.V1.Ledger.Value    as LedgerValueV1
import qualified Plutus.V1.Ledger.Address  as LedgerAddressV1
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified PlutusTx.Builtins         as PlutusTxBuiltins

import qualified Data.Aeson                as DataAeson
import qualified GHC.Generics              as GHCGenerics
import qualified Prelude                   as P

import qualified Helpers.OnChain           as HelpersOnChain
import qualified Helpers.OnChain as OnChainHelpers

import qualified Plutus.V1.Ledger.Address as Ledger
import qualified Helpers.Types as T
import Helpers.Types (SimpleSale(SimpleSale))

data RightsNFT_PolicyRedeemerMintIDType = RightsNFT_PolicyRedeemerMintIDType

instance Eq RightsNFT_PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    RightsNFT_PolicyRedeemerMintIDType == RightsNFT_PolicyRedeemerMintIDType = True

PlutusTx.unstableMakeIsData ''RightsNFT_PolicyRedeemerMintIDType

data RightsNFT_PolicyRedeemerBurnIDType = RightsNFT_PolicyRedeemerBurnIDType

instance Eq RightsNFT_PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    RightsNFT_PolicyRedeemerBurnIDType == RightsNFT_PolicyRedeemerBurnIDType = True

PlutusTx.unstableMakeIsData ''RightsNFT_PolicyRedeemerBurnIDType

data RightsNFT_PolicyRedeemer
    = RightsNFT_PolicyRedeemerMintID RightsNFT_PolicyRedeemerMintIDType
    | RightsNFT_PolicyRedeemerBurnID RightsNFT_PolicyRedeemerBurnIDType

instance Eq RightsNFT_PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    RightsNFT_PolicyRedeemerMintID rmtx1 == RightsNFT_PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    RightsNFT_PolicyRedeemerBurnID rmtx1 == RightsNFT_PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''RightsNFT_PolicyRedeemer
    [ ('RightsNFT_PolicyRedeemerMintID, 1),
      ('RightsNFT_PolicyRedeemerBurnID, 2)
    ]

{-# INLINABLE mkRightsNFT_Policy #-}
mkRightsNFT_Policy ::  LedgerApiV2.ValidatorHash -> BuiltinData -> BuiltinData -> ()
mkRightsNFT_Policy callOptionValidatorHash redRaw ctxRaw =
    let
        redeemer = LedgerApiV2.unsafeFromBuiltinData @RightsNFT_PolicyRedeemer redRaw
        ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        info = LedgerContextsV2.scriptContextTxInfo ctx

        callOptionValidatorAddress = scriptHashAddress callOptionValidatorHash

        getHashDatumInput :: LedgerContextsV2.TxOut
        getHashDatumInput = case hashDatumInputs of
                        [o] -> o
                        _   -> traceError "expected exactly one input"
            where
                hashDatumInputs :: [LedgerContextsV2.TxOut]
                hashDatumInputs = 
                    [ txOut | txOut <- LedgerApiV2.txInfoOutputs info,
                        let address = LedgerApiV2.txOutAddress txOut
                        in OnChainHelpers.isScriptAddress address && address == callOptionValidatorAddress
                    ]

        datum :: SimpleSale
        datum = case T.parseCallOptionDatumInput getHashDatumInput info of
            Nothing -> traceError "Datum not found"
            Just x  -> x
    in
        if case redeemer of
                RightsNFT_PolicyRedeemerMintID _ -> mintRightNFTPolicy datum ctx
                RightsNFT_PolicyRedeemerBurnID _ -> burnRightNFTPolicy datum ctx
        then ()
        else error ()

{-# INLINEABLE burnRightNFTPolicy #-}
burnRightNFTPolicy :: T.SimpleSale -> LedgerContextsV2.ScriptContext -> Bool
burnRightNFTPolicy datum_In ctx =
    traceIfFalse "The Token Minted has a bad name" rightTokenName &&
    traceIfFalse "Bad amount burned" checkBurnAmount 
  where
    rightsNFT_Policy_CS = LedgerContextsV2.ownCurrencySymbol ctx
    info = LedgerContextsV2.scriptContextTxInfo ctx

    mintedAmount :: Integer
    mintedAmount = LedgerValueV1.assetClassValueOf (LedgerContextsV2.txInfoMint info) (T.dRightsNFT datum_In)

    checkBurnAmount :: Bool
    checkBurnAmount = mintedAmount == (-1)

    rightsNFT_Policy_TN nftAC = 
      LedgerApiV2.TokenName $ PlutusTxBuiltins.blake2b_256 $ HelpersOnChain.assetClassToBBS nftAC

    rightTokenName :: Bool
    rightTokenName = assetClass rightsNFT_Policy_CS (rightsNFT_Policy_TN (T.dSellingNFT datum_In)) == T.dRightsNFT datum_In

{-# INLINEABLE mintRightNFTPolicy #-}
mintRightNFTPolicy :: T.SimpleSale -> LedgerContextsV2.ScriptContext -> Bool
mintRightNFTPolicy datum_In ctx =
    traceIfFalse "The Token Minted has a bad name" rightTokenName &&
    traceIfFalse "Bad amount minted" checkMintAmount 
  where
    rightsNFT_Policy_CS = LedgerContextsV2.ownCurrencySymbol ctx
    info = LedgerContextsV2.scriptContextTxInfo ctx

    rightsNFT_Policy_TN nftAC = 
      LedgerApiV2.TokenName $ PlutusTxBuiltins.blake2b_256 $ HelpersOnChain.assetClassToBBS nftAC

    mintedAmount :: Integer
    mintedAmount = LedgerValueV1.assetClassValueOf (LedgerContextsV2.txInfoMint info) (T.dRightsNFT datum_In)

    checkMintAmount :: Bool
    checkMintAmount = mintedAmount == 1

    rightTokenName :: Bool
    rightTokenName = assetClass rightsNFT_Policy_CS (rightsNFT_Policy_TN (T.dSellingNFT datum_In)) == T.dRightsNFT datum_In

{-# INLINEABLE rightsNFT_Policy #-}
rightsNFT_Policy ::  LedgerApiV2.ValidatorHash -> LedgerApiV2.MintingPolicy
rightsNFT_Policy datumHash = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ rightsNFT_PlutonomyPolicy datumHash

{-# INLINEABLE rightsNFT_PlutonomyPolicy #-}
rightsNFT_PlutonomyPolicy :: LedgerApiV2.ValidatorHash -> Plutonomy.MintingPolicy
rightsNFT_PlutonomyPolicy datumHash =
  Plutonomy.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkRightsNFT_Policy ||])
            `PlutusTx.applyCode` PlutusTx.liftCode datumHash

