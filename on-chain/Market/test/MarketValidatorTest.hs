{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (Monad (return), replicateM, unless, void)
import PlutusTx.Prelude (Bool (False, True), Eq ((==)), Maybe (..), ($), (&&), (+), (++), (-), (.))
import Prelude (IO, Semigroup ((<>)), Show (show), mconcat)

import Codec.Serialise (Serialise, serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS

import qualified Ledger.Value as LedgerValue
import qualified Plutus.Model as Model
import qualified Plutus.V1.Ledger.Value as LedgerValueV1
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Test.Tasty as Tasty

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as ApiShelley
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Builtins.Internal as BuiltinsInternal

import Types

import Policys.PolicyID
import Policys.PolicyNFT
import Validators.MarketValidator

import Debug.Trace (traceM)
import qualified Helpers.OffChain as HelpersOffChain
import qualified Helpers.OnChain as HelpersOnChain

---------------------------------------------------------------------------------------------------
------------------------------------------ TESTING ------------------------------------------------
type IsGood = Bool

main :: IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing validator with some sensible situations"
        [ Tasty.testGroup
            "End to End situations"
            [ good "Minting NFT works" testMintNFT
            , good "Selling the NFT works" testSellNFT
            , good "Buy Call Option works" testBuyCallOption
            , good "Withdraw Call Option works" testWithdraw
            ]
        , Tasty.testGroup
            "Must fail situations"
            [ good "Minting the same NFT twice fails" testMintNFTTwice
            , good "Selling the NFT with bad token name" testBadSellNFT
            , good "Buy Call Option without pay" testBuyCallOptionWithoutPay
            , good "Try to get back the call option of someone else" testTryStoleWithdraw
            ]
        ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000_000) Model.defaultBabbage

---------------------------------------------------------------------------------------------------
----------------------------- HELPER FUNCTIONS/INSTANCES/TYPES ------------------------------------
serializableToScript :: Serialise a => a -> Api.PlutusScript Api.PlutusScriptV2
serializableToScript = ApiShelley.PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- Serialize validator
validatorToScript :: LedgerApiV2.Validator -> Api.PlutusScript Api.PlutusScriptV2
validatorToScript = serializableToScript

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

validatorHash' :: LedgerApiV2.Validator -> LedgerApiV2.ValidatorHash
validatorHash' = LedgerApiV2.ValidatorHash . BuiltinByteString . Api.serialiseToRawBytes . hashScript . validatorToScript

-- Set many users at once
setupUsers :: Model.Run [LedgerApiV2.PubKeyHash]
setupUsers = replicateM 4 $ Model.newUser $ Model.ada (Model.Lovelace 1_000_000_000)

--------------------------------------------------------------------------------
--------------------- TESTING MINTING NFT --------------------------------------
-- NFT Minting Policy's script
-- nftScript :: LedgerApiV2.TxOutRef -> Model.TypedPolicy NFTRedeemer
nftScript ref = Model.TypedPolicy . Model.toV2 $ policy_NFT ref

mintNFTTx :: LedgerApiV2.TxOutRef -> LedgerApiV2.TxOut -> LedgerApiV2.Value -> LedgerApiV2.PubKeyHash -> Model.Tx
mintNFTTx ref out val pkh =
    mconcat
        [ Model.mintValue (nftScript ref) Mint val
        , Model.payToKey pkh $ val <> LedgerApiV2.txOutValue out
        , Model.spendPubKey ref
        ]

mintNFT :: LedgerApiV2.PubKeyHash -> Model.Run LedgerValueV1.AssetClass
mintNFT u = do
    utxos <- Model.utxoAt u
    let [(ref, out)] = utxos
        currSymbol = Model.scriptCurrencySymbol (nftScript ref)
        mintingValue = LedgerApiV2.singleton currSymbol "NFT" 1
    Model.submitTx u $ mintNFTTx ref out mintingValue u
    v1 <- Model.valueAt u
    -- traceM $ "values es: " ++ show v1
    unless (v1 == Model.adaValue 1000000000 <> mintingValue) $
        Model.logError "Final balances are incorrect"
    return $ LedgerValueV1.assetClass currSymbol "NFT"

testMintNFT :: Model.Run ()
testMintNFT = do
    [u1, _, _, _] <- setupUsers
    void $ mintNFT u1

testMintNFTTwice :: Model.Run ()
testMintNFTTwice = do
    [u1, _, _, _] <- setupUsers
    utxos <- Model.utxoAt u1
    let [(ref, out)] = utxos
        mintingValue = LedgerApiV2.singleton (Model.scriptCurrencySymbol (nftScript ref)) "NFT" 1
        tx = mintNFTTx ref out mintingValue u1
    Model.submitTx u1 tx
    Model.submitTx u1 tx
    v1 <- Model.valueAt u1
    unless (v1 == Model.adaValue 1000000000 <> mintingValue) $
        Model.logError "Final balances are incorrect"

----------------------------------------------------------------------------------------------------
-------------------------------------- TESTING NFT Right -------------------------------------------
type ContractValidator = Model.TypedValidator SimpleSale MarketRedeemer

type PolicyID = Model.TypedPolicy PolicyIDRedeemer

-- nft minting policy's script
contractValidator :: ContractValidator
contractValidator = Model.TypedValidator . Model.toV2 $ marketValidator

policyIDScript :: LedgerApiV2.ValidatorHash -> PolicyID
policyIDScript callOptionValidatorHash = Model.TypedPolicy . Model.toV2 $ policy_ID callOptionValidatorHash

sellNFTx :: Model.UserSpend -> SimpleSale -> PolicyID -> LedgerApiV2.Value -> Model.Tx
sellNFTx sp dat policyId val =
    mconcat
        [ Model.userSpend sp
        , -- , Model.refInputInline callOptionValidatorHash
          Model.mintValue policyId (PolicyRedeemerMintID PolicyRedeemerMintIDType) (LedgerValueV1.assetClassValue (policyID dat) 1)
        , Model.payToScript contractValidator (Model.InlineDatum dat) val
        ]

--
sellNFT :: IsGood -> LedgerApiV2.PubKeyHash -> LedgerValueV1.AssetClass -> Model.Run (ContractValidator, PolicyID, SimpleSale)
sellNFT isGood u1 nftAC = do
    let nftV = LedgerValueV1.assetClassValue nftAC 1
        callOptionValidatorHash = validatorHash' marketValidator
        policyId = policyIDScript callOptionValidatorHash
        ---------------------
        policyID_CS = Model.scriptCurrencySymbol policyId
        policyID_AC = LedgerValue.AssetClass (policyID_CS, marketID_TN)
        policyID_Mint_Value = LedgerValueV1.assetClassValue policyID_AC 1
        ---------------------
        callOptionUTxO_Value = nftV <> policyID_Mint_Value
        ---------------------
        datum =
            SimpleSale{sellerAddress = u1, policyID = policyID_AC, sellingToken = nftAC, priceOfAsset = 5_000_000}
        userSpendValue = nftV 
        mintingTxValues = policyID_Mint_Value

    sp <- Model.spend u1 userSpendValue
    let tx = sellNFTx sp datum policyId (userSpendValue <> mintingTxValues)
    -- traceM $ show tx
    Model.submitTx u1 tx

    v1 <- Model.valueAt u1
    utxos1 <- Model.valueAt contractValidator
    -- traceM $ "v1 " ++ show v1
    -- traceM $ "utxo1 " ++ show utxos1
    --
    -- traceM $ "v1 " ++ show ( Model.adaValue (1000000000 - callOptionUTxO_MinADA))
    -- traceM $ "utxo1 " ++ show ( Model.adaValue 1 <> rightsNFT_Mint_Value <> nftV)
    -- traceM $ "utxo2 " ++ show ( Model.adaValue (dMinADA datum))
    unless
        ( v1 == Model.adaValue (1000000000)
            && utxos1 == (userSpendValue <> mintingTxValues)
        )
        $ Model.logError "Final balances are incorrect"
    return
        ( contractValidator
        , policyId
        , datum
        )

--
testSellNFT :: Model.Run ()
testSellNFT = do
    [u1, _, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    void $ sellNFT True u1 nftAC

testBadSellNFT :: Model.Run ()
testBadSellNFT = do
    [u1, _, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    void $ sellNFT False u1 nftAC

--
-- ----------------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING Buy Call Option -------------------------------------------

buyNFTTx :: Model.UserSpend -> SimpleSale -> ContractValidator -> LedgerApiV2.TxOutRef -> LedgerApiV2.TxOutRef -> Model.Tx
buyNFTTx sp datum validator ref datumRef =
    mconcat
        [ Model.userSpend sp
        , Model.spendScript validator ref Buy datum
        , Model.refInputInline datumRef
        , Model.payToKey (sellerAddress datum) $ Model.adaValue (priceOfAsset datum)
        , Model.payToScript validator (Model.InlineDatum datum) (LedgerValueV1.assetClassValue (sellingToken datum) 1)
        ]

buyNFT :: LedgerApiV2.PubKeyHash -> ContractValidator -> SimpleSale -> Model.Run ()
buyNFT user contract datum = do
    [(ref, _)] <- Model.utxoAt contract
    sp <- Model.spend user $ Model.adaValue (priceOfAsset datum)

    timeRange <- Model.currentTimeRad 100
    [(refDatum, _)] <- Model.utxoAt contract
    tx <- Model.validateIn timeRange $ buyNFTTx sp datum contract ref refDatum 
    Model.submitTx user tx

    v1 <- Model.valueAt (sellerAddress datum)
    v2 <- Model.valueAt user

    -- traceM $ "va1 " ++ show v1
    -- traceM $ "v21 " ++ show v2
    -- traceM $ "utxo1 " ++ show utxo1
    -- traceM $ "utxo2 " ++ show utxo2
    unless
        ( v1 == Model.adaValue (1000000000 + priceOfAsset datum)
            && v2 == Model.adaValue (1000000000 - priceOfAsset datum) <> LedgerValueV1.assetClassValue (sellingToken datum) 1
        )
        $ Model.logError "Final balances are incorrect"

testBuyCallOption :: Model.Run ()
testBuyCallOption = do
    [u1, u2, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    (contract, _, datum) <- sellNFT True u1 nftAC
    -- Buy Call Options
    void $ buyNFT u2 contract datum

buyNFTWithoutPayTx :: LedgerApiV2.PubKeyHash -> SimpleSale -> ContractValidator -> LedgerApiV2.TxOutRef -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> Model.Tx
buyNFTWithoutPayTx buyer datum validator ref datumRef rightNFT =
    mconcat
        [ Model.spendScript validator ref Buy datum
        , Model.refInputInline datumRef
        , Model.payToKey buyer rightNFT
        , Model.payToScript validator (Model.InlineDatum datum) (LedgerValueV1.assetClassValue (sellingToken datum) 1)
        ]

buyNFTWithoutPay :: LedgerApiV2.PubKeyHash -> ContractValidator -> SimpleSale -> Model.Run ()
buyNFTWithoutPay user contract datum = do
    [(ref, _)] <- Model.utxoAt contract
    [(refDatum, _)] <- Model.utxoAt contract
    let rightNFT = LedgerValueV1.assetClassValue (sellingToken datum) 1
        tx = buyNFTWithoutPayTx user datum contract ref refDatum rightNFT
    Model.submitTx user tx

    v1 <- Model.valueAt (sellerAddress datum)
    v2 <- Model.valueAt user
    utxo <- Model.valueAt contract
    unless
        ( v1 == Model.adaValue (1000000000)
            && utxo == LedgerValueV1.assetClassValue (sellingToken datum) 1
            && v2 == Model.adaValue 1000000000 <> LedgerValueV1.assetClassValue (sellingToken datum) 1
        )
        $ Model.logError "Final balances are incorrect"

testBuyCallOptionWithoutPay :: Model.Run ()
testBuyCallOptionWithoutPay = do
    [u1, u2, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    (contract,  _, datum) <- sellNFT True u1 nftAC
    -- Buy Call Options
    void $ buyNFTWithoutPay u2 contract datum

-- ----------------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING Get back call option---------------------------------------
--
withdrawTx :: SimpleSale -> ContractValidator -> LedgerApiV2.TxOutRef -> LedgerApiV2.TxOutRef -> Model.Tx
withdrawTx datum contract ref datumRef =
    mconcat
        [ Model.spendScript contract ref Withdraw datum
        , Model.refInputInline datumRef
        , Model.payToKey (sellerAddress datum) $ LedgerValueV1.assetClassValue (sellingToken datum) 1
        ]

exerciseWithdraw :: IsGood -> LedgerApiV2.PubKeyHash -> ContractValidator -> SimpleSale -> Model.Run ()
exerciseWithdraw isGood user contract datum = do
    [(ref, _)] <- Model.utxoAt contract
    [(refDatum, _)] <- Model.utxoAt contract
    timeRange <- Model.currentTimeRad 100
    tx <-
        Model.validateIn timeRange $
            if isGood
                then withdrawTx datum contract ref refDatum
                else occupiedWithdrawTx datum contract ref refDatum
    Model.submitTx user tx

    v1 <- Model.valueAt (sellerAddress datum)
    if isGood
        then
            unless
                (v1 == Model.adaValue 1000000000 <> LedgerValueV1.assetClassValue (sellingToken datum) 1)
                $ Model.logError "Final balances are incorrect"
        else
            unless
                (v1 == Model.adaValue (1000000000 + priceOfAsset datum) <> LedgerValueV1.assetClassValue (sellingToken datum) 1)
                $ Model.logError "Final balances are incorrect"

testWithdraw :: Model.Run ()
testWithdraw = do
    [u1, _, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    (contract, _, datum) <- sellNFT True u1 nftAC
    -- Withdraw Call Options
    void $ exerciseWithdraw True u1 contract datum 

occupiedWithdrawTx :: SimpleSale -> ContractValidator -> LedgerApiV2.TxOutRef -> LedgerApiV2.TxOutRef -> Model.Tx
occupiedWithdrawTx datum contract ref datumRef =
    mconcat
        [ Model.spendScript contract ref Withdraw datum
        , Model.refInputInline datumRef
        , Model.payToKey (sellerAddress datum) $ LedgerValueV1.assetClassValue (sellingToken datum) 1
        ]

testOccupiedWithdraw :: Model.Run ()
testOccupiedWithdraw = do
    [u1, u2, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    (contract, policyNft, datum) <- sellNFT True u1 nftAC
    -- Buy Call Options
    _ <- buyNFT u2 contract datum
    -- Withdraw Call Options
    void $ exerciseWithdraw False u1 contract datum 


testTryStoleWithdraw :: Model.Run ()
testTryStoleWithdraw = do
    [u1, u2, _, _] <- setupUsers
    -- Mint NFT
    nftAC <- mintNFT u1
    -- Sell NFT
    (contract, policyNft, datum) <- sellNFT True u1 nftAC
    -- Withdraw Call Options
    void $ exerciseWithdraw True u2 contract datum 
