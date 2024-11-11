import { Assets, Data, Tx } from 'lucid-cardano';
import { NextApiResponse } from 'next';
import {
    addAssetsList,
    addressToPubKeyHash,
    BackEndApiHandlersFor,
    BackEndAppliedFor,
    BaseSmartDBBackEndApiHandlers,
    BaseSmartDBBackEndApplied,
    BaseSmartDBBackEndMethods,
    calculateMinAdaOfUTxO,
    console_error,
    console_log,
    isEmulator,
    LucidToolsBackEnd,
    Maybe,
    NextApiRequestAuthenticated,
    objToCborHex,
    optionsGetMinimalWithSmartUTxOCompleteFields,
    sanitizeForDatabase,
    showData,
    strToHex,
    TimeBackEnd,
    TRANSACTION_STATUS_PENDING,
    TransactionBackEndApplied,
    TransactionDatum,
    TransactionEntity,
    TransactionRedeemer,
    WalletTxParams,
    walletTxParamsSchema,
    yup,
} from 'smart-db/backEnd';
import {
    BuyMarketNFTTxParams,
    buyMarketNFTTxParamsSchema,
    MARKET_BUY,
    MARKET_SELL,
    MARKET_WITHDRAW,
    SellMarketNFTTxParams,
    sellMarketNFTTxParamsSchema,
    WithdrawMarketNFTTxParams,
    withdrawMarketNFTTxParamsSchema,
} from '../../Commons/Constants/transactions';
import { MarketNFTEntity } from '../Entities/MarketNFT.Entity';
import { Buy, PolicyRedeemerBurnID, PolicyRedeemerMintID, Withdraw } from '../Entities/Redeemers/MarketNFT.Redeemer';

@BackEndAppliedFor(MarketNFTEntity)
export class MarketNFTBackEndApplied extends BaseSmartDBBackEndApplied {
    protected static _Entity = MarketNFTEntity;
    protected static _BackEndMethods = BaseSmartDBBackEndMethods;
}

@BackEndApiHandlersFor(MarketNFTEntity)
export class MarketNFTApiHandlers extends BaseSmartDBBackEndApiHandlers {
    protected static _Entity = MarketNFTEntity;
    protected static _BackEndApplied = MarketNFTBackEndApplied;
    // #region custom api handlers

    protected static _ApiHandlers: string[] = ['tx'];

    protected static async executeApiHandlers(command: string, req: NextApiRequestAuthenticated, res: NextApiResponse) {
        //--------------------
        const { query } = req.query;
        //--------------------
        if (this._ApiHandlers.includes(command) && query !== undefined) {
            if (query[0] === 'tx') {
                if (query.length === 2) {
                    if (query[1] === 'sell-nft-tx') {
                        return await this.sellTxApiHandler(req, res);
                    } else if (query[1] === 'buy-marketNft-tx') {
                        return await this.buyTxApiHandler(req, res);
                    } else if (query[1] === 'withdraw-nft-tx') {
                        return await this.withdrawTxApiHandler(req, res);
                    }
                    return res.status(405).json({ error: 'Wrong Api route' });
                } else {
                    console_error(0, this._Entity.className(), `executeApiHandlers - Error: Api Handler function not found`);
                    return res.status(500).json({
                        error: 'Api Handler function not found ',
                    });
                }
            } else {
                console_error(0, this._Entity.className(), `executeApiHandlers - Error: Wrong Custom Api route`);
                return res.status(405).json({ error: 'Wrong Custom Api route ' });
            }
        }

        // #endregion custom api handlers
    }

    public static async sellTxApiHandler(req: NextApiRequestAuthenticated, res: NextApiResponse) {
        //--------------------
        if (req.method === 'POST') {
            console_log(1, this._Entity.className(), `Sell Tx - POST - Init`);
            try {
                //-------------------------
                const sanitizedBody = sanitizeForDatabase(req.body);
                //-------------------------
                const schemaBody = yup.object().shape({
                    walletTxParams: walletTxParamsSchema,
                    txParams: sellMarketNFTTxParamsSchema,
                });
                //-------------------------
                let validatedBody;
                try {
                    validatedBody = await schemaBody.validate(sanitizedBody);
                } catch (error) {
                    console_error(-1, this._Entity.className(), `Create Tx - Error: ${error}`);
                    return res.status(400).json({ error });
                }
                //--------------------------------------
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: SellMarketNFTTxParams;
                } = sanitizedBody; //--------------------------------------
                console_log(0, this._Entity.className(), `Sell Tx - txParams: ${showData(txParams)}`);
                //--------------------------------------
                if (isEmulator) {
                    // solo en emulator. Me aseguro de setear el emulador al tiempo real del server. Va a saltear los slots necesarios.
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }
                //--------------------------------------
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);
                //--------------------------------------
                const { utxos: uTxOsAtWallet, address } = walletTxParams;
                //--------------------------------------
                const { priceOfAsset, token_TN, token_CS, datumID_CS, datumID_TN, validatorAddress, mintingPolicyID } = txParams;
                //--------------------------------------
                const lucidAC_MintID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Mint_ID: Assets = { [lucidAC_MintID]: 1n };
                console_log(0, this._Entity.className(), `Sell Tx - valueFor_Mint_ID: ${showData(valueFor_Mint_ID)}`);
                //----------------------------
                let valueFor_MarketNFTDatum_Out: Assets = valueFor_Mint_ID;
                // const minADA_For_MarketNFTDatum = calculateMinAdaOfUTxO({
                //   assets: valueFor_MarketNFTDatum_Out,
                // });
                // const value_MinAda_For_MarketNFTDatum: Assets = {
                //   lovelace: minADA_For_MarketNFTDatum,
                // };
                // valueFor_MarketNFTDatum_Out = addAssetsList([value_MinAda_For_MarketNFTDatum, valueFor_MarketNFTDatum_Out]);
        //
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerToken: Assets = { [lucidAC_sellerToken]: 1n };
                valueFor_MarketNFTDatum_Out = addAssetsList([valueOfSellerToken, valueFor_MarketNFTDatum_Out]);
                console_log(0, this._Entity.className(), `Sell Tx - valueFor_Mint_ID: ${showData(valueFor_Mint_ID)}`);
                console_log(0, this._Entity.className(), `Sell Tx - valueFor_FundDatum_Out: ${showData(valueFor_MarketNFTDatum_Out, false)}`);
                //--------------------------------------
                const paymentPKH = addressToPubKeyHash(address);
                const datumPlainObject = {
                    sellerAddress: paymentPKH,
                    policyID: lucidAC_MintID,
                    sellingToken: lucidAC_sellerToken,
                    priceOfAsset: BigInt(priceOfAsset),
                };
                //--------------------------------------
                let marketNftDatum_Out = MarketNFTEntity.mkDatumFromPlainObject(datumPlainObject);
                console_log(0, this._Entity.className(), `Sell Tx - marketNftDatum_Out: ${showData(marketNftDatum_Out, false)}`);
                const marketNftDatum_Out_Hex = MarketNFTEntity.datumToCborHex(marketNftDatum_Out);
                console_log(0, this._Entity.className(), `Sell Tx - marketNftDatum_Out_Hex: ${showData(marketNftDatum_Out_Hex, false)}`);
                //--------------------------------------
                const marketNftPolicyRedeemerMintID = new PolicyRedeemerMintID();
                console_log(0, this._Entity.className(), `Sell Tx - marketNftPolicyRedeemerMintID: ${showData(marketNftPolicyRedeemerMintID, false)}`);
                const marketNftPolicyRedeemerMintID_Hex = objToCborHex(marketNftPolicyRedeemerMintID);
                console_log(0, this._Entity.className(), `Sell Tx - marketNftPolicyRedeemerMintID_Hex: ${showData(marketNftPolicyRedeemerMintID_Hex, false)}`);
                //--------------------------------------
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();
                console_log(0, this._Entity.className(), `Sell Tx - from ${from} to ${until}`);
                //--------------------------------------
                let tx: Tx = lucid.newTx();
                //--------------------------------------
                tx = tx
                    .mintAssets(valueFor_Mint_ID, marketNftPolicyRedeemerMintID_Hex)
                    .payToContract(validatorAddress, { inline: marketNftDatum_Out_Hex }, valueFor_MarketNFTDatum_Out)
                    .attachMintingPolicy(mintingPolicyID);
                //--------------------------------------
                const txComplete = await tx.complete();
                //--------------------------------------
                const txCborHex = txComplete.toString();
                //--------------------------------------
                const txHash = txComplete.toHash();
                //--------------------------------------
                const transactionMarketNFTPolicyRedeemerMintID: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'mint',
                    redeemerObj: marketNftPolicyRedeemerMintID,
                };
                //--------------------------------------
                const transactionMarketNFTDatum_Out: TransactionDatum = {
                    address: validatorAddress,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNftDatum_Out,
                };
                //--------------------------------------
                const transaction: TransactionEntity = new TransactionEntity({
                    paymentPKH: walletTxParams.pkh,
                    date: new Date(now),
                    type: MARKET_SELL,
                    hash: txHash,
                    status: TRANSACTION_STATUS_PENDING,
                    ids: {},
                    redeemers: {
                        marketNftPolicyRedeemerMintID: transactionMarketNFTPolicyRedeemerMintID,
                    },
                    datums: { marketNftDatum_Out: transactionMarketNFTDatum_Out },
                    consuming_UTxOs: [],
                });
                await TransactionBackEndApplied.create(transaction);
                //--------------------------------------
                console_log(-1, this._Entity.className(), `Sell Tx - txCborHex: ${showData(txCborHex)}`);
                return res.status(200).json({ txCborHex });
                //--------------------------------------
            } catch (error) {
                console_error(-1, this._Entity.className(), `Sell Tx - Error: ${error}`);
                return res.status(500).json({
                    error: `An error occurred while creating the ${this._Entity.apiRoute()} Sell Tx: ${error}`,
                });
            }
        } else {
            console_error(-1, this._Entity.className(), `Sell Tx - Error: Method not allowed`);
            return res.status(405).json({ error: `Method not allowed` });
        }
    }

    public static async buyTxApiHandler(req: NextApiRequestAuthenticated, res: NextApiResponse) {
        //--------------------
        if (req.method === 'POST') {
            console_log(1, this._Entity.className(), `Buy Tx - POST - Init`);
            try {
                //-------------------------
                const sanitizedBody = sanitizeForDatabase(req.body);
                //-------------------------
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: BuyMarketNFTTxParams;
                } = sanitizedBody;
                //--------------------------------------
                console_log(0, this._Entity.className(), `Buy Tx - txParams: ${showData(txParams)}`);
                //--------------------------------------
                if (isEmulator) {
                    // solo en emulator. Me aseguro de setear el emulador al tiempo real del server. Va a saltear los slots necesarios.
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }
                //--------------------------------------
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);
                //--------------------------------------
                const { utxos: uTxOsAtWallet, address } = walletTxParams;
                //--------------------------------------
                const { priceOfAsset, token_TN, token_CS, datumID_CS, datumID_TN, marketNft_id, sellerAddress, mintingPolicyID, validatorMarket } = txParams;
                //--------------------------------------
                const marketNft = await MarketNFTBackEndApplied.getById_<MarketNFTEntity>(marketNft_id, {
                    ...optionsGetMinimalWithSmartUTxOCompleteFields,
                });
                if (marketNft === undefined) {
                    throw `Invalid marketNft id`;
                }
                //--------------------------------------
                const marketNft_SmartUTxO = marketNft.smartUTxO;
                if (marketNft_SmartUTxO === undefined) {
                    throw `Can't find MarketNFT UTxO`;
                }
                if (marketNft_SmartUTxO.unsafeIsAvailableForConsuming() === false) {
                    throw `MarketNFT UTxO is being used, please wait and try again`;
                }
                //--------------------------------------
                const marketNft_UTxO = marketNft_SmartUTxO.getUTxO();
                //--------------------------------------
                const lucidAC_BurnID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Burn_ID: Assets = { [lucidAC_BurnID]: -1n };
                console_log(0, this._Entity.className(), `Buy Tx - valueFor_Burn_ID: ${showData(valueFor_Burn_ID)}`);
                //----------------------------
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerToken: Assets = { [lucidAC_sellerToken]: 1n };
                console_log(0, this._Entity.className(), `Buy Tx - valueFor_SellToken: ${showData(valueOfSellerToken)}`);
                //----------------------------
                const value_TokenPrice_For_MarketNFTDatum: Assets = {
                    lovelace: priceOfAsset,
                };
                //----------------------------
                const marketNftPolicyRedeemerBurnID = new PolicyRedeemerBurnID();
                console_log(0, this._Entity.className(), `Buy Tx - marketNftPolicyRedeemerBurnID: ${showData(marketNftPolicyRedeemerBurnID, false)}`);
                const marketNftPolicyRedeemerBurnID_Hex = objToCborHex(marketNftPolicyRedeemerBurnID);
                console_log(0, this._Entity.className(), `Buy Tx - marketNftPolicyRedeemerBurnID_Hex: ${showData(marketNftPolicyRedeemerBurnID_Hex, false)}`);
                //--------------------------------------
                const marketNftValidatorRedeemerBuy = new Buy();
                console_log(0, this._Entity.className(), `Buy Tx - marketNftValidatorRedeemerBuy: ${showData(marketNftValidatorRedeemerBuy, false)}`);
                const marketNftValidatorRedeemerBuy_Hex = objToCborHex(marketNftValidatorRedeemerBuy);
                console_log(0, this._Entity.className(), `Buy Tx - marketNftValidatorRedeemerBuy_Hex: ${showData(marketNftValidatorRedeemerBuy_Hex, false)}`);
                //--------------------------------------
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();
                console_log(0, this._Entity.className(), `Buy Tx - from ${from} to ${until}`);
                //--------------------------------------
                let tx: Tx = lucid.newTx();
                //--------------------------------------
                tx = await tx
                    .mintAssets(valueFor_Burn_ID, marketNftPolicyRedeemerBurnID_Hex)
                    .collectFrom([marketNft_UTxO], marketNftValidatorRedeemerBuy_Hex)
                    .attachMintingPolicy(mintingPolicyID)
                    .attachSpendingValidator(validatorMarket)
                    .payToAddress(sellerAddress, value_TokenPrice_For_MarketNFTDatum)
                    .payToAddress(address, valueOfSellerToken)
                    .addSigner(address);
                //----------------------------
                const txComplete = await tx.complete();
                //--------------------------------------
                const txCborHex = txComplete.toString();
                //--------------------------------------
                const txHash = txComplete.toHash();
                //--------------------------------------
                const transactionMarketNFTPolicyRedeemerBurnID: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'mint',
                    redeemerObj: marketNftPolicyRedeemerBurnID,
                };
                //--------------------------------------
                const transactionMarketNFTValidatorRedeemerBuy: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'spend',
                    redeemerObj: marketNftValidatorRedeemerBuy,
                };
                //--------------------------------------
                const transactionMarketNFTDatum_In: TransactionDatum = {
                    address: marketNft_SmartUTxO.address,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNft_SmartUTxO.datumObj,
                };
                //--------------------------------------
                const transaction: TransactionEntity = new TransactionEntity({
                    paymentPKH: walletTxParams.pkh,
                    date: new Date(now),
                    type: MARKET_BUY,
                    hash: txHash,
                    status: TRANSACTION_STATUS_PENDING,
                    ids: {},
                    redeemers: {
                        marketNftPolicyRedeemerBurnID: transactionMarketNFTPolicyRedeemerBurnID,
                        marketNftValidatorRedeemerBuy: transactionMarketNFTValidatorRedeemerBuy,
                    },
                    datums: { marketNftDatum_In: transactionMarketNFTDatum_In },
                    consuming_UTxOs: [marketNft_UTxO],
                });
                await TransactionBackEndApplied.create(transaction);
                //--------------------------------------
                console_log(-1, this._Entity.className(), `Buy Tx - txCborHex: ${showData(txCborHex)}`);
                return res.status(200).json({ txCborHex });
                //--------------------------------------
            } catch (error) {
                console_error(-1, this._Entity.className(), `Buy Tx - Error: ${error}`);
                return res.status(500).json({
                    error: `An error occurred while creating the ${this._Entity.apiRoute()} Buy Tx: ${error}`,
                });
            }
        } else {
            console_error(-1, this._Entity.className(), `Buy Tx - Error: Method not allowed`);
            return res.status(405).json({ error: `Method not allowed` });
        }
    }

    public static async withdrawTxApiHandler(req: NextApiRequestAuthenticated, res: NextApiResponse) {
        //--------------------
        if (req.method === 'POST') {
            console_log(1, this._Entity.className(), `Withdraw Tx - POST - Init`);
            try {
                //-------------------------
                const sanitizedBody = sanitizeForDatabase(req.body);
                //-------------------------
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: WithdrawMarketNFTTxParams;
                } = sanitizedBody;
                //--------------------------------------
                console_log(0, this._Entity.className(), `Withdraw Tx - txParams: ${showData(txParams)}`);
                //--------------------------------------
                if (isEmulator) {
                    // solo en emulator. Me aseguro de setear el emulador al tiempo real del server. Va a saltear los slots necesarios.
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }
                //--------------------------------------
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);
                //--------------------------------------
                const { utxos: uTxOsAtWallet, address } = walletTxParams;
                //--------------------------------------
                const { token_TN, token_CS, datumID_CS, datumID_TN, marketNft_id, mintingPolicyID, validatorMarket } = txParams;
                //--------------------------------------
                const marketNft = await MarketNFTBackEndApplied.getById_<MarketNFTEntity>(marketNft_id, {
                    ...optionsGetMinimalWithSmartUTxOCompleteFields,
                });
                if (marketNft === undefined) {
                    throw `Invalid marketNft id`;
                }
                //--------------------------------------
                const marketNft_SmartUTxO = marketNft.smartUTxO;
                if (marketNft_SmartUTxO === undefined) {
                    throw `Can't find MarketNFT UTxO`;
                }
                if (marketNft_SmartUTxO.unsafeIsAvailableForConsuming() === false) {
                    throw `MarketNFT UTxO is being used, please wait and try again`;
                }
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerToken: Assets = { [lucidAC_sellerToken]: 1n };
                console_log(0, this._Entity.className(), `Withdraw Tx - valueFor_SellToken: ${showData(valueOfSellerToken)}`);
                //--------------------------------------
                const marketNft_UTxO = marketNft_SmartUTxO.getUTxO();
                //--------------------------------------
                const lucidAC_BurnID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Burn_ID: Assets = { [lucidAC_BurnID]: -1n };
                console_log(0, this._Entity.className(), `Withdraw Tx - valueFor_Burn_ID: ${showData(valueFor_Burn_ID)}`);
                //----------------------------
                const marketNftPolicyRedeemerBurnID = new PolicyRedeemerBurnID();
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftPolicyRedeemerBurnID: ${showData(marketNftPolicyRedeemerBurnID, false)}`);
                const marketNftPolicyRedeemerBurnID_Hex = objToCborHex(marketNftPolicyRedeemerBurnID);
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftPolicyRedeemerBurnID_Hex: ${showData(marketNftPolicyRedeemerBurnID_Hex, false)}`);
                //--------------------------------------
                const marketNftValidatorRedeemerWithdraw = new Withdraw();
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftValidatorRedeemerWithdraw: ${showData(marketNftValidatorRedeemerWithdraw, false)}`);
                const marketNftValidatorRedeemerWithdraw_Hex = objToCborHex(marketNftValidatorRedeemerWithdraw);
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftValidatorRedeemerWithdraw_Hex: ${showData(marketNftValidatorRedeemerWithdraw_Hex, false)}`);
                //--------------------------------------
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();
                console_log(0, this._Entity.className(), `Withdraw Tx - from ${from} to ${until}`);
                //--------------------------------------
                let tx: Tx = lucid.newTx();
                //--------------------------------------
                tx = await tx
                    .mintAssets(valueFor_Burn_ID, marketNftPolicyRedeemerBurnID_Hex)
                    .collectFrom([marketNft_UTxO], marketNftValidatorRedeemerWithdraw_Hex)
                    .attachMintingPolicy(mintingPolicyID)
                    .attachSpendingValidator(validatorMarket)
                    .payToAddress(address, valueOfSellerToken)
                    .addSigner(address);

                //----------------------------
                const txComplete = await tx.complete();
                //--------------------------------------
                const txCborHex = txComplete.toString();
                //--------------------------------------
                const txHash = txComplete.toHash();
                //--------------------------------------
                const transactionMarketNFTPolicyRedeemerBurnID: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'mint',
                    redeemerObj: marketNftPolicyRedeemerBurnID,
                };
                //--------------------------------------
                const transactionMarketNFTValidatorRedeemerWithdraw: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'spend',
                    redeemerObj: marketNftValidatorRedeemerWithdraw,
                };
                //--------------------------------------
                const transactionMarketNFTDatum_In: TransactionDatum = {
                    address: marketNft_SmartUTxO.address,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNft_SmartUTxO.datumObj,
                };
                //--------------------------------------
                const transaction: TransactionEntity = new TransactionEntity({
                    paymentPKH: walletTxParams.pkh,
                    date: new Date(now),
                    type: MARKET_WITHDRAW,
                    hash: txHash,
                    status: TRANSACTION_STATUS_PENDING,
                    ids: {},
                    redeemers: {
                        marketNftPolicyRedeemerBurnID: transactionMarketNFTPolicyRedeemerBurnID,
                        marketNftValidatorRedeemerWithdraw: transactionMarketNFTValidatorRedeemerWithdraw,
                    },
                    datums: { marketNftDatum_In: transactionMarketNFTDatum_In },
                    consuming_UTxOs: [marketNft_UTxO],
                });
                await TransactionBackEndApplied.create(transaction);
                //--------------------------------------
                console_log(-1, this._Entity.className(), `Withdraw Tx - txCborHex: ${showData(txCborHex)}`);
                return res.status(200).json({ txCborHex });
                //--------------------------------------
            } catch (error) {
                console_error(-1, this._Entity.className(), `Withdraw Tx - Error: ${error}`);
                return res.status(500).json({
                    error: `An error occurred while creating the ${this._Entity.apiRoute()} Withdraw Tx: ${error}`,
                });
            }
        } else {
            console_error(-1, this._Entity.className(), `Withdraw Tx - Error: Method not allowed`);
            return res.status(405).json({ error: `Method not allowed` });
        }
    }
}
