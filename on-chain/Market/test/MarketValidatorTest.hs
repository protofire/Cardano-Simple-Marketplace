{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad (Monad (return), replicateM, unless, void)
import PlutusTx.Prelude (Bool (True), Eq ((==)), ($), (&&), (+), (-), (.), fst)
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
import PlutusTx.Builtins.Internal as BuiltinsInternal (
  BuiltinByteString (BuiltinByteString),
 )

import Types (MarketRedeemer (..), SimpleSale (..), marketID_TN)

import Debug.Trace (traceM)
import Policys.PolicyID (
  PolicyIDRedeemer (..),
  PolicyRedeemerBurnIDType (PolicyRedeemerBurnIDType),
  PolicyRedeemerMintIDType (PolicyRedeemerMintIDType),
  policy_ID,
 )

import Policys.PolicyNFT (NFTRedeemer (Mint), policy_NFT)
import Validators.MarketValidator (marketValidator)

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
        , good "Buy NFT works" testBuyNFT
        , good "Withdraw Call Option works" testWithdraw
        ]
    , Tasty.testGroup
        "Must fail situations"
        [ bad "Minting the same NFT twice fails" testMintNFTTwice
        , bad "Buy NFT without pay" testBuyNFTWithoutPay
        , bad "Try to get back the call option of someone else" testTryStoleWithdraw
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

tokenNameNFT :: LedgerApiV2.TokenName
tokenNameNFT = "NFT"

policyIdAc :: SimpleSale -> LedgerValueV1.AssetClass
policyIdAc datum = LedgerValue.assetClass (policyID_CS datum) marketID_TN

sellingTokenAc :: SimpleSale -> LedgerValueV1.AssetClass
sellingTokenAc datum = LedgerValue.assetClass (sellingToken_CS datum) (sellingToken_TN datum)

--------------------------------------------------------------------------------
--------------------- TESTING MINTING NFT --------------------------------------
-- NFT Minting Policy's script
nftScript :: LedgerApiV2.TxOutRef -> Model.TypedPolicy NFTRedeemer
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
      mintingValue = LedgerApiV2.singleton currSymbol tokenNameNFT 1
  Model.submitTx u $ mintNFTTx ref out mintingValue u
  v1 <- Model.valueAt u
  -- traceM $ "values es: " ++ show v1
  unless (v1 == Model.adaValue 1000000000 <> mintingValue) $
    Model.logError "Final balances are incorrect"
  return $ LedgerValueV1.assetClass currSymbol tokenNameNFT

testMintNFT :: Model.Run ()
testMintNFT = do
  [u1, _, _, _] <- setupUsers
  void $ mintNFT u1

testMintNFTTwice :: Model.Run ()
testMintNFTTwice = do
  [u1, _, _, _] <- setupUsers
  utxos <- Model.utxoAt u1
  let [(ref, out)] = utxos
      mintingValue = LedgerApiV2.singleton (Model.scriptCurrencySymbol (nftScript ref)) tokenNameNFT 1
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
sellNFTx sp dat policy val =
  mconcat
    [ Model.userSpend sp
    , Model.mintValue policy (PolicyRedeemerMintID PolicyRedeemerMintIDType) (LedgerValueV1.assetClassValue (policyIdAc dat) 1)
    , Model.payToScript contractValidator (Model.InlineDatum dat) val
    ]

--
sellNFT :: LedgerApiV2.PubKeyHash -> LedgerValueV1.AssetClass -> Model.Run (ContractValidator, PolicyID, SimpleSale)
sellNFT u1 nftAC = do
  let nftV = LedgerValueV1.assetClassValue nftAC 1
      callOptionValidatorHash = validatorHash' marketValidator
      policyId = policyIDScript callOptionValidatorHash
      -- now = 2000
      ---------------------
      policyID_CurrSym = Model.scriptCurrencySymbol policyId
      policyID_AC = LedgerValue.AssetClass (policyID_CurrSym, marketID_TN)
      policyID_Mint_Value = LedgerValueV1.assetClassValue policyID_AC 1
      ---------------------
      sellingToken_CurrSym = fst (LedgerValueV1.unAssetClass nftAC)
      ---------------------
      callOptionUTxO_Value = nftV <> policyID_Mint_Value
      callOptionUTxO_MinADA = HelpersOnChain.calculateMinADAOfValue callOptionUTxO_Value True
      minAdaValue = Model.adaValue callOptionUTxO_MinADA
      ---------------------
      ---------------------
      datum =
        SimpleSale
          { sellerAddress = u1
          , policyID_CS = policyID_CurrSym
          , sellingToken_CS = sellingToken_CurrSym
          , sellingToken_TN = tokenNameNFT
          , priceOfAsset = 5_000_000
          , minADA = callOptionUTxO_MinADA
          }
      userSpendValue = nftV <> minAdaValue
      mintingTxValues = policyID_Mint_Value

  sp <- Model.spend u1 userSpendValue
  let tx = sellNFTx sp datum policyId (userSpendValue <> mintingTxValues)
  -- traceM $ show datum
  Model.submitTx u1 tx

  v1 <- Model.valueAt u1
  utxos1 <- Model.valueAt contractValidator

  -- traceM $ show v1
  -- traceM $ show utxos1
  unless
    ( v1 == Model.adaValue (1000000000 - minADA datum)
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
  void $ sellNFT u1 nftAC

--------------------------------------------------------------------------------
---------------------- TESTING Exercise Call Option ----------------------------
data CaseExerciseCallOption = BuyNFT | BuyNFTWithoutPay

buyNFTx :: CaseExerciseCallOption -> Model.UserSpend -> LedgerApiV2.PubKeyHash -> SimpleSale -> ContractValidator -> PolicyID -> LedgerApiV2.TxOutRef -> Model.Tx
buyNFTx option sp buyer datum contract policy ref =
  case option of
    BuyNFT ->
      mconcat
        [ Model.userSpend sp
        , Model.spendScript contract ref Buy datum
        , Model.payToKey (sellerAddress datum) $ Model.adaValue (priceOfAsset datum + minADA datum)
        , Model.payToKey buyer (LedgerValueV1.assetClassValue (sellingTokenAc datum) 1)
        , Model.mintValue policy (PolicyRedeemerBurnID PolicyRedeemerBurnIDType) (LedgerValueV1.assetClassValue (policyIdAc datum) (-1))
        ]
    BuyNFTWithoutPay ->
      mconcat
        [ Model.spendScript contract ref Buy datum
        , Model.payToKey buyer (LedgerValueV1.assetClassValue (sellingTokenAc datum) 1 <> Model.adaValue (minADA datum))
        , Model.mintValue policy (PolicyRedeemerBurnID PolicyRedeemerBurnIDType) (LedgerValueV1.assetClassValue (policyIdAc datum) (-1))
        ]

buyNFT :: CaseExerciseCallOption -> LedgerApiV2.PubKeyHash -> ContractValidator -> PolicyID -> SimpleSale -> Model.Run ()
buyNFT option user contract policy datum = do
  [(ref, _)] <- Model.utxoAt contract
  sp <-
    Model.spend user $
      Model.adaValue (priceOfAsset datum)

  timeRange <- Model.currentTimeRad 100
  tx <- Model.validateIn timeRange $ buyNFTx option sp user datum contract policy ref
  Model.submitTx user tx

  v1 <- Model.valueAt (sellerAddress datum)
  v2 <- Model.valueAt user

  unless
    ( v1 == Model.adaValue (1000000000 + priceOfAsset datum)
        && v2 == Model.adaValue (1000000000 - priceOfAsset datum) <> LedgerValueV1.assetClassValue (sellingTokenAc datum) 1
    )
    $ Model.logError "Final balances are incorrect"

testBuyNFT :: Model.Run ()
testBuyNFT = do
  [u1, u2, _, _] <- setupUsers
  -- Mint NFT
  nftAC <- mintNFT u1
  -- Sell NFT
  (contract, policy, datum) <- sellNFT u1 nftAC
  -- Buy NFTs
  void $ buyNFT BuyNFT u2 contract policy datum

testBuyNFTWithoutPay :: Model.Run ()
testBuyNFTWithoutPay = do
  [u1, u2, _, _] <- setupUsers
  -- Mint NFT
  nftAC <- mintNFT u1
  -- Buy NFTs
  (contract, policy, datum) <- sellNFT u1 nftAC
  -- Buy NFTs
  -- Buy NFT
  void $ buyNFT BuyNFTWithoutPay u2 contract policy datum

-- ----------------------------------------------------------------------------------------------------------
-- -------------------------------------- TESTING Get back call option---------------------------------------
data CaseWithdraw = WithdrawCase | WithdrawOcuppedCase

getBackTx :: CaseWithdraw -> SimpleSale -> ContractValidator -> PolicyID -> LedgerApiV2.TxOutRef -> Model.Tx
getBackTx option datum contract policy ref =
  case option of
    WithdrawCase ->
      mconcat
        [ Model.spendScript contract ref Withdraw datum
        , Model.payToKey
            (sellerAddress datum)
            (LedgerValueV1.assetClassValue (sellingTokenAc datum) 1 <> Model.adaValue (minADA datum))
        , Model.mintValue
            policy
            (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)
            (LedgerValueV1.assetClassValue (policyIdAc datum) (-1))
        ]
    WithdrawOcuppedCase ->
      mconcat
        [ Model.spendScript contract ref Withdraw datum
        , Model.payToKey (sellerAddress datum) $ LedgerValueV1.assetClassValue (sellingTokenAc datum) 1 <> Model.adaValue (minADA datum)
        , Model.mintValue policy (PolicyRedeemerBurnID PolicyRedeemerBurnIDType) (LedgerValueV1.assetClassValue (policyIdAc datum) (-1))
        ]

exerciseWithdraw :: CaseWithdraw -> LedgerApiV2.PubKeyHash -> ContractValidator -> PolicyID -> SimpleSale -> Model.Run ()
exerciseWithdraw isOcuped user contract policy datum = do
  [(ref, _)] <- Model.utxoAt contract
  timeRange <- Model.currentTimeRad 100
  tx <- Model.validateIn timeRange $ getBackTx isOcuped datum contract policy ref
  Model.submitTx user tx

  v1 <- Model.valueAt (sellerAddress datum)
  case isOcuped of
    WithdrawCase ->
      unless
        ( v1
            == Model.adaValue 1000000000
              <> LedgerValueV1.assetClassValue (sellingTokenAc datum) 1
        )
        $ Model.logError "Final balances are incorrect"
    WithdrawOcuppedCase ->
      unless
        ( v1
            == Model.adaValue (1000000000 + minADA datum)
              <> LedgerValueV1.assetClassValue (sellingTokenAc datum) 1
        )
        $ Model.logError "Final balances are incorrect"

testWithdraw :: Model.Run ()
testWithdraw = do
  [u1, _, _, _] <- setupUsers
  -- Mint NFT
  nftAC <- mintNFT u1
  -- Sell NFT
  (contract, policy, datum) <- sellNFT u1 nftAC
  -- Get Back Call Options
  void $ exerciseWithdraw WithdrawCase u1 contract policy datum

testTryStoleWithdraw :: Model.Run ()
testTryStoleWithdraw = do
  [u1, u2, _, _] <- setupUsers
  -- Mint NFT
  nftAC <- mintNFT u1
  -- Sell NFT
  (contract, policy, datum) <- sellNFT u1 nftAC
  -- Get Back Call Options
  void $ exerciseWithdraw WithdrawCase u2 contract policy datum
