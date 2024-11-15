// Import essential modules and types from Lucid (Cardano library), Next.js, and Smart DB for backend API handling and utilities
import { Assets, Tx } from 'lucid-cardano';
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
} from 'smart-db/backEnd';
import { BuyMarketNFTTxParams, MARKET_BUY, MARKET_SELL, MARKET_WITHDRAW, SellMarketNFTTxParams, WithdrawMarketNFTTxParams } from '../../Commons/Constants/transactions';
import { MarketNFTEntity } from '../Entities/MarketNFT.Entity';
import { Buy, PolicyRedeemerBurnID, PolicyRedeemerMintID, Withdraw } from '../Entities/Redeemers/MarketNFT.Redeemer';
import { marketPlaceVersion } from '../../Commons/Constants/onchain';

// Define a class that applies Smart DB API handlers to MarketNFTEntity
@BackEndAppliedFor(MarketNFTEntity)
export class MarketNFTBackEndApplied extends BaseSmartDBBackEndApplied {
    protected static _Entity = MarketNFTEntity;
    protected static _BackEndMethods = BaseSmartDBBackEndMethods;
}

// Define API handlers for MarketNFT transactions (sell, buy, withdraw)
@BackEndApiHandlersFor(MarketNFTEntity)
export class MarketNFTApiHandlers extends BaseSmartDBBackEndApiHandlers {
    protected static _Entity = MarketNFTEntity;
    protected static _BackEndApplied = MarketNFTBackEndApplied;

    protected static _ApiHandlers: string[] = ['tx']; // Define the supported API handler 'tx'

    // Function to execute API handlers based on commands received
    protected static async executeApiHandlers(command: string, req: NextApiRequestAuthenticated, res: NextApiResponse) {
        const { query } = req.query;

        if (this._ApiHandlers.includes(command) && query !== undefined) {
            if (query[0] === 'tx') {
                // Check for different transaction types: sell, buy, and withdraw
                if (query.length === 2) {
                    if (query[1] === 'sell-nft-tx') {
                        return await this.sellTxApiHandler(req, res);
                    } else if (query[1] === 'buy-nft-tx') {
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
    }

    // Handler to create a Sell NFT transaction
    public static async sellTxApiHandler(req: NextApiRequestAuthenticated, res: NextApiResponse) {
        if (req.method === 'POST') {
            console_log(1, this._Entity.className(), `Sell Tx - POST - Init`);
            try {
                const sanitizedBody = sanitizeForDatabase(req.body);

                // Destructure required parameters from the request body
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: SellMarketNFTTxParams;
                } = sanitizedBody;

                console_log(0, this._Entity.className(), `Sell Tx - txParams: ${showData(txParams)}`);

                // Emulator sync for development environment only
                if (isEmulator) {
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }

                // Prepare Lucid for transaction handling and wallet info
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);
                const { utxos: uTxOsAtWallet, address } = walletTxParams;

                // Extract transaction parameters related to the asset for sale
                const { priceOfAsset, token_TN, token_CS, datumID_CS, datumID_TN, validatorAddress, mintingPolicyID } = txParams;

                const paymentPKH = addressToPubKeyHash(address);

                // Generate datum object with relevant sale data and no min ADA yet
                const datumPlainObject_NoMinADA = {
                    version: marketPlaceVersion,
                    sellerPaymentPKH: paymentPKH,
                    policyID_CS: datumID_CS,
                    sellingToken_CS: token_CS,
                    sellingToken_TN: strToHex(token_TN),
                    priceOfAsset: BigInt(priceOfAsset),
                    minADA: BigInt(0),
                };

                const lucidAC_MintID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Mint_ID: Assets = { [lucidAC_MintID]: 1n };

                let valueFor_MarketNFTDatum_Out: Assets = valueFor_Mint_ID;
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerToken: Assets = { [lucidAC_sellerToken]: 1n };

                // Add additional values to the transaction, including minimum ADA requirement
                valueFor_MarketNFTDatum_Out = addAssetsList([valueOfSellerToken, valueFor_MarketNFTDatum_Out]);
                const minADA_For_MarketNFTDatum = calculateMinAdaOfUTxO({
                    datum: MarketNFTEntity.datumToCborHex(datumPlainObject_NoMinADA),
                    assets: valueFor_MarketNFTDatum_Out,
                });
                const value_MinAda_For_MarketNFTDatum: Assets = {
                    lovelace: minADA_For_MarketNFTDatum,
                };
                valueFor_MarketNFTDatum_Out = addAssetsList([value_MinAda_For_MarketNFTDatum, valueFor_MarketNFTDatum_Out]);

                // Generate datum object with min ADA calculated
                const datumPlainObject = {
                    ...datumPlainObject_NoMinADA,
                    minADA: BigInt(minADA_For_MarketNFTDatum),
                };

                // Create and encode the datum for the transaction
                let marketNftDatum_Out = MarketNFTEntity.mkDatumFromPlainObject(datumPlainObject);
                const marketNftDatum_Out_Hex = MarketNFTEntity.datumToCborHex(marketNftDatum_Out);

                // Create minting policy and redeemers for the sale transaction
                const marketNftPolicyRedeemerMintID = new PolicyRedeemerMintID();
                const marketNftPolicyRedeemerMintID_Hex = objToCborHex(marketNftPolicyRedeemerMintID);

                // Time range setup for the transaction
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();

                let tx: Tx = lucid.newTx();
                tx = tx
                    .mintAssets(valueFor_Mint_ID, marketNftPolicyRedeemerMintID_Hex)
                    .payToContract(validatorAddress, { inline: marketNftDatum_Out_Hex }, valueFor_MarketNFTDatum_Out)
                    .attachMintingPolicy(mintingPolicyID);

                const txComplete = await tx.complete();
                const txCborHex = txComplete.toString();
                const txHash = txComplete.toHash();

                // Create and save transaction entity in the Smart DB
                const transactionMarketNFTPolicyRedeemerMintID: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'mint',
                    redeemerObj: marketNftPolicyRedeemerMintID,
                };

                const transactionMarketNFTDatum_Out: TransactionDatum = {
                    address: validatorAddress,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNftDatum_Out,
                };

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

                return res.status(200).json({ txCborHex, txHash });
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
                // Sanitize the request body before proceeding to database operations
                const sanitizedBody = sanitizeForDatabase(req.body);
                //-------------------------
                // Destructure walletTxParams and txParams from the sanitized body
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: BuyMarketNFTTxParams;
                } = sanitizedBody;
                //--------------------------------------
                // Log txParams data to debug and confirm the values
                console_log(0, this._Entity.className(), `Buy Tx - txParams: ${showData(txParams)}`);
                //--------------------------------------
                // Check if running in emulator mode, adjust time synchronization if necessary
                if (isEmulator) {
                    // For emulator only, ensure it syncs with server time, skipping the necessary slots
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }
                //--------------------------------------
                // Prepare Lucid backend for transaction creation using the wallet's transaction parameters
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);
                //--------------------------------------
                // Extract necessary wallet and transaction details
                const { utxos: uTxOsAtWallet, address } = walletTxParams;
                //--------------------------------------
                // Extract additional transaction parameters needed for buying the NFT
                const { token_TN, token_CS, datumID_CS, datumID_TN, marketNft_id, sellerAddress: sellerPaymentPKH, mintingPolicyID, validatorMarket } = txParams;
                //--------------------------------------
                // Fetch the market NFT entity from the database by its ID
                const marketNft = await MarketNFTBackEndApplied.getById_<MarketNFTEntity>(marketNft_id, {
                    ...optionsGetMinimalWithSmartUTxOCompleteFields,
                });
                if (marketNft === undefined) {
                    throw `Invalid marketNft id`;
                }
                //--------------------------------------
                // Retrieve SmartUTxO for the fetched market NFT and ensure it is available
                const marketNft_SmartUTxO = marketNft.smartUTxO;
                if (marketNft_SmartUTxO === undefined) {
                    throw `Can't find MarketNFT UTxO`;
                }
                if (marketNft_SmartUTxO.unsafeIsAvailableForConsuming() === false) {
                    throw `MarketNFT UTxO is being used, please wait and try again`;
                }
                //--------------------------------------
                // Get the UTxO of the market NFT from its SmartUTxO
                const marketNft_UTxO = marketNft_SmartUTxO.getUTxO();
                //--------------------------------------
                // Prepare the values for burning and minting the required assets in the transaction
                const lucidAC_BurnID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Burn_ID: Assets = { [lucidAC_BurnID]: -1n };
                console_log(0, this._Entity.className(), `Buy Tx - valueFor_Burn_ID: ${showData(valueFor_Burn_ID)}`);
                //----------------------------
                // Prepare the value for the seller's token in the transaction
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerToken: Assets = { [lucidAC_sellerToken]: 1n };
                console_log(0, this._Entity.className(), `Buy Tx - valueFor_SellToken: ${showData(valueOfSellerToken)}`);
                //----------------------------
                // Prepare the value representing the total price of the NFT plus any minimal ADA required for the transaction
                const value_TokenPrice_PlusMinADA_For_MarketNFTDatum: Assets = {
                    lovelace: BigInt(marketNft.priceOfAsset + marketNft.minADA),
                };
                //----------------------------
                // Create the PolicyRedeemerBurnID used for the burn operation
                const marketNftPolicyRedeemerBurnID = new PolicyRedeemerBurnID();
                console_log(0, this._Entity.className(), `Buy Tx - marketNftPolicyRedeemerBurnID: ${showData(marketNftPolicyRedeemerBurnID, false)}`);
                const marketNftPolicyRedeemerBurnID_Hex = objToCborHex(marketNftPolicyRedeemerBurnID);
                console_log(0, this._Entity.className(), `Buy Tx - marketNftPolicyRedeemerBurnID_Hex: ${showData(marketNftPolicyRedeemerBurnID_Hex, false)}`);
                //--------------------------------------
                // Create the redeemer object for the buy operation, which will be used in the validator
                const marketNftValidatorRedeemerBuy = new Buy();
                console_log(0, this._Entity.className(), `Buy Tx - marketNftValidatorRedeemerBuy: ${showData(marketNftValidatorRedeemerBuy, false)}`);
                const marketNftValidatorRedeemerBuy_Hex = objToCborHex(marketNftValidatorRedeemerBuy);
                console_log(0, this._Entity.className(), `Buy Tx - marketNftValidatorRedeemerBuy_Hex: ${showData(marketNftValidatorRedeemerBuy_Hex, false)}`);
                //--------------------------------------
                // Get the time range for the transaction (to prevent replay attacks)
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();
                console_log(0, this._Entity.className(), `Buy Tx - from ${from} to ${until}`);
                //--------------------------------------
                // Start building the transaction with Lucid
                let tx: Tx = lucid.newTx();
                //--------------------------------------
                // Add various components to the transaction, including minting, spending, and payment operations
                tx = tx
                    .mintAssets(valueFor_Burn_ID, marketNftPolicyRedeemerBurnID_Hex) // Mint assets to burn the token
                    .collectFrom([marketNft_UTxO], marketNftValidatorRedeemerBuy_Hex) // Collect the market NFT UTxO
                    .attachMintingPolicy(mintingPolicyID) // Attach the minting policy
                    .attachSpendingValidator(validatorMarket) // Attach the spending validator
                    .payToAddress(sellerPaymentPKH, value_TokenPrice_PlusMinADA_For_MarketNFTDatum) // Pay the seller the price + ADA
                    .payToAddress(address, valueOfSellerToken) // Send the buyer their token
                    .addSigner(address); // Add the wallet address as a signer
                //----------------------------
                // Complete the transaction and get the CBOR hex representation of the transaction
                const txComplete = await tx.complete();
                //--------------------------------------
                const txCborHex = txComplete.toString();
                //--------------------------------------
                // Get the transaction hash for reference
                const txHash = txComplete.toHash();
                console_log(0, this._Entity.className(), `Buy Tx - txHash: ${showData(txHash)}`);
                //--------------------------------------
                // Create transaction redeemer objects for the burn and buy operations
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
                // Create transaction datum objects to track the market NFT's datum and input
                const transactionMarketNFTDatum_In: TransactionDatum = {
                    address: marketNft_SmartUTxO.address,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNft_SmartUTxO.datumObj,
                };
                //--------------------------------------
                // Save the transaction to the backend, marking it as pending until confirmed
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
                // Log the final transaction CBOR hex data and return it in the response
                console_log(-1, this._Entity.className(), `Buy Tx - txCborHex: ${showData(txCborHex)}`);
                return res.status(200).json({ txCborHex, txHash });
                //--------------------------------------
            } catch (error) {
                // Handle errors and respond with a 500 status code
                console_error(-1, this._Entity.className(), `Buy Tx - Error: ${error}`);
                return res.status(500).json({
                    error: `An error occurred while creating the ${this._Entity.apiRoute()} Buy Tx: ${error}`,
                });
            }
        } else {
            // If method is not POST, respond with method not allowed
            console_error(-1, this._Entity.className(), `Buy Tx - Error: Method not allowed`);
            return res.status(405).json({ error: `Method not allowed` });
        }
    }

    public static async withdrawTxApiHandler(req: NextApiRequestAuthenticated, res: NextApiResponse) {
        // Checks if the HTTP method is POST to handle the withdrawal transaction
        if (req.method === 'POST') {
            console_log(1, this._Entity.className(), `Withdraw Tx - POST - Init`);

            try {
                // Sanitizes the incoming request body to prevent potential database-related security issues
                const sanitizedBody = sanitizeForDatabase(req.body);

                // Destructures `walletTxParams` and `txParams` from the sanitized request body
                const {
                    walletTxParams,
                    txParams,
                }: {
                    walletTxParams: WalletTxParams;
                    txParams: WithdrawMarketNFTTxParams;
                } = sanitizedBody;

                // Logs the transaction parameters for debugging
                console_log(0, this._Entity.className(), `Withdraw Tx - txParams: ${showData(txParams)}`);

                // Ensures synchronization of the blockchain with server time if running in emulator mode
                if (isEmulator) {
                    // Uncomment this line to synchronize the emulator with server time
                    // await TimeBackEnd.syncBlockChainWithServerTime()
                }

                // Prepares the Lucid instance for transaction processing
                const { lucid } = await LucidToolsBackEnd.prepareLucidBackEndForTx(walletTxParams);

                // Extracts UTxOs and address from wallet transaction parameters
                const { utxos: uTxOsAtWallet, address } = walletTxParams;

                // Extracts specific parameters required for processing the transaction
                const { token_TN, token_CS, datumID_CS, datumID_TN, marketNft_id, mintingPolicyID, validatorMarket } = txParams;

                // Retrieves the Market NFT associated with the transaction based on the provided ID
                const marketNft = await MarketNFTBackEndApplied.getById_<MarketNFTEntity>(marketNft_id, {
                    ...optionsGetMinimalWithSmartUTxOCompleteFields,
                });

                // Throws an error if the Market NFT is not found
                if (marketNft === undefined) {
                    throw `Invalid marketNft id`;
                }

                // Checks that the Market NFT has an associated smart UTxO and is available for consumption
                const marketNft_SmartUTxO = marketNft.smartUTxO;
                if (marketNft_SmartUTxO === undefined) {
                    throw `Can't find MarketNFT UTxO`;
                }
                if (marketNft_SmartUTxO.unsafeIsAvailableForConsuming() === false) {
                    throw `MarketNFT UTxO is being used, please wait and try again`;
                }

                // Constructs asset values for seller tokens plus ADA and logs it
                const lucidAC_sellerToken = token_CS + strToHex(token_TN);
                const valueOfSellerTokenPlusADA: Assets = {
                    [lucidAC_sellerToken]: 1n,
                    lovelace: BigInt(marketNft.priceOfAsset + marketNft.minADA),
                };
                console_log(0, this._Entity.className(), `Withdraw Tx - valueFor_SellToken: ${showData(valueOfSellerTokenPlusADA)}`);

                // Gets the UTxO associated with the Market NFT
                const marketNft_UTxO = marketNft_SmartUTxO.getUTxO();

                // Constructs the asset value for burning ID tokens and logs it
                const lucidAC_BurnID = datumID_CS + strToHex(datumID_TN);
                const valueFor_Burn_ID: Assets = { [lucidAC_BurnID]: -1n };
                console_log(0, this._Entity.className(), `Withdraw Tx - valueFor_Burn_ID: ${showData(valueFor_Burn_ID)}`);

                // Creates a redeemer for the burning policy, converts it to CBOR format, and logs it
                const marketNftPolicyRedeemerBurnID = new PolicyRedeemerBurnID();
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftPolicyRedeemerBurnID: ${showData(marketNftPolicyRedeemerBurnID, false)}`);
                const marketNftPolicyRedeemerBurnID_Hex = objToCborHex(marketNftPolicyRedeemerBurnID);
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftPolicyRedeemerBurnID_Hex: ${showData(marketNftPolicyRedeemerBurnID_Hex, false)}`);

                // Creates a redeemer for the validator and converts it to CBOR format for transaction withdrawal
                const marketNftValidatorRedeemerWithdraw = new Withdraw();
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftValidatorRedeemerWithdraw: ${showData(marketNftValidatorRedeemerWithdraw, false)}`);
                const marketNftValidatorRedeemerWithdraw_Hex = objToCborHex(marketNftValidatorRedeemerWithdraw);
                console_log(0, this._Entity.className(), `Withdraw Tx - marketNftValidatorRedeemerWithdraw_Hex: ${showData(marketNftValidatorRedeemerWithdraw_Hex, false)}`);

                // Sets the transaction time range and logs it
                const { now, from, until } = await TimeBackEnd.getTxTimeRange();
                console_log(0, this._Entity.className(), `Withdraw Tx - from ${from} to ${until}`);

                // Initializes a new Lucid transaction object
                let tx: Tx = lucid.newTx();

                // Configures transaction actions: mint, collect, attach policies, and send funds
                tx = tx
                    .mintAssets(valueFor_Burn_ID, marketNftPolicyRedeemerBurnID_Hex)
                    .collectFrom([marketNft_UTxO], marketNftValidatorRedeemerWithdraw_Hex)
                    .attachMintingPolicy(mintingPolicyID)
                    .attachSpendingValidator(validatorMarket)
                    .payToAddress(address, valueOfSellerTokenPlusADA)
                    .addSigner(address);

                // Completes the transaction preparation
                const txComplete = await tx.complete();

                // Converts the transaction to CBOR Hex and computes the hash
                const txCborHex = txComplete.toString();
                const txHash = txComplete.toHash();
                console_log(0, this._Entity.className(), `Withdraw Tx - txHash: ${showData(txHash)}`);

                // Creates transaction redeemer entities for record-keeping
                const transactionMarketNFTPolicyRedeemerBurnID: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'mint',
                    redeemerObj: marketNftPolicyRedeemerBurnID,
                };

                const transactionMarketNFTValidatorRedeemerWithdraw: TransactionRedeemer = {
                    tx_index: 0,
                    purpose: 'spend',
                    redeemerObj: marketNftValidatorRedeemerWithdraw,
                };

                // Defines the input datum for the transaction
                const transactionMarketNFTDatum_In: TransactionDatum = {
                    address: marketNft_SmartUTxO.address,
                    datumType: MarketNFTEntity.className(),
                    datumObj: marketNft_SmartUTxO.datumObj,
                };

                // Creates and stores a new transaction entity in the backend
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

                // Logs the transaction CBOR Hex and returns it in the response
                console_log(-1, this._Entity.className(), `Withdraw Tx - txCborHex: ${showData(txCborHex)}`);
                return res.status(200).json({ txCborHex, txHash });
            } catch (error) {
                // Logs any errors encountered and sends a 500 response with the error message
                console_error(-1, this._Entity.className(), `Withdraw Tx - Error: ${error}`);
                return res.status(500).json({
                    error: `An error occurred while creating the ${this._Entity.apiRoute()} Withdraw Tx: ${error}`,
                });
            }
        } else {
            // Handles unsupported HTTP methods with a 405 response
            console_error(-1, this._Entity.className(), `Withdraw Tx - Error: Method not allowed`);
            return res.status(405).json({ error: `Method not allowed` });
        }
    }
}
