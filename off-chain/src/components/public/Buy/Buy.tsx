import { useContext, useState } from 'react';

import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import { AppStateContext } from '@/pages/_app';
import { mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { BuyMarketNFTTxParams, WithdrawMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { Credential } from 'lucid-cardano';
import Modal from 'react-modal';
import {
    BaseSmartDBFrontEndBtnHandlers,
    formatTokenNameHexToStr,
    getUrlForImage,
    hexToStr,
    isValidUrl,
    pushSucessNotification,
    pushWarningNotification,
    TOKEN_ICON_GENERIC,
    TokenMetadataEntity,
    TokenMetadataFrontEndApiCalls,
    useList,
    useTokensStore,
    useWalletStore,
} from 'smart-db';

type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

export const Buy = () => {
    //----------------------------------------------------------------------------
    const walletStore = useWalletStore();
    const tokensStore = useTokensStore();
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext);
    const { marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript, mintingPolicyID_CS } = appState;
    //----------------------------------------------------------------------------
    const [isLoading, setIsLoading] = useState(false);
    const [isLoadingTxBuy, setIsLoadingTxBuy] = useState(false);
    const [isLoadingTxWithdraw, setIsLoadingTxWithdraw] = useState(false);
    const [isLoadingSync, setIsLoadingSync] = useState(false);
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false);
    const [txHash, setTxHash] = useState<string>();
    const [isTxError, setIsTxError] = useState(false);
    const [txMessage, setTxMessage] = useState('');
    const [txConfirmed, setTxConfirmed] = useState(false);
    //----------------------------------------------------------------------------
    const loadList = async () => {
        //--------
        const listEntities: MarketNFTEntity[] = await MarketNFTApi.getAllApi_({
            fieldsForSelect: {},
            loadRelations: { smartUTxO_id: true },
        });
        const listTokensWithMetadata: MarketNFTEntityWithMetadata[] = [];
        if (listEntities.length === 0) return [];
        const listTokens = listEntities.map((item) => {
            return { CS: item.sellingToken_CS, TN_Hex: item.sellingToken_TN };
        });
        const listMetadata = await TokenMetadataFrontEndApiCalls.get_Tokens_MetadataApi(listTokens);
        for (const item of listEntities) {
            const metadata = listMetadata.find((x) => x.CS === item.sellingToken_CS && x.TN_Hex === item.sellingToken_TN);
            if (metadata !== undefined) {
                listTokensWithMetadata.push({ entity: item, metadata });
            }
        }
        return listTokensWithMetadata;
    };
    //--------------------------------------
    const { isLoadingList, isLoadedList, list, listDivRef, current, refreshList, setCurrent, handlePageChange, currentPage, setTotalPages, totalPages, addPagination } =
        useList<MarketNFTEntityWithMetadata>({
            nameList: MarketNFTEntity.className(),
            loadList,
        });
    //--------------------------------------
    const handleBtnSync = async () => {
        //----------------------------
        if (marketAddress === undefined) return;
        //----------------------------
        setIsLoadingSync(true);
        //----------------------------
        try {
            //----------------------------
            await MarketNFTApi.syncWithAddressApi(MarketNFTEntity, marketAddress, true);
            refreshList();
            //----------------------------
            pushSucessNotification(`MarketNFT Sync`, 'Syncronization complete!', false);
            //----------------------------
        } catch (e) {
            console.error(e);
            pushWarningNotification(`MarketNFT Sync`, 'Syncronization Error' + e);
        }
        //----------------------------
        setIsLoadingSync(false);
    };
    //--------------------------------------
    const handleBtnBuyTx = async (item: MarketNFTEntityWithMetadata) => {
        //----------------------------
        if (walletStore.isConnected !== true) return;
        if (marketAddress === undefined) return;
        if (marketScript === undefined) return;
        if (mintingPolicyIDScript === undefined) return;
        if (mintingPolicyID_CS === undefined) return;
        //----------------------------
        setIsTxModalOpen(true);
        //----------------------------
        if (isLoadingTxBuy) {
            return;
        }
        //----------------------------
        setIsLoadingTxBuy(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(item.entity.sellingToken_TN);
        const token_CS = item.entity.sellingToken_CS;
        try {
            //----------------------------
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            //----------------------------
            // Convert the public key hash to a payment credential
            const paymentCredential: Credential = { type: 'Key', hash: item.entity.sellerPaymentPKH };
            const address = (await walletStore.getLucid())!.utils.credentialToAddress(paymentCredential);
            //--------------------------------------
            const txParams: BuyMarketNFTTxParams = {
                marketNft_id: item.entity._DB_id,
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                sellerAddress: address,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(item.entity.priceOfAsset),
            };
            //--------------------------------------
            const result = await BaseSmartDBFrontEndBtnHandlers.handleBtnDoTransactionV1(
                MarketNFTEntity,
                'Buy MarketNFT...',
                'Buy Tx',
                setTxMessage,
                setTxHash,
                walletStore,
                txParams,
                MarketNFTApi.callGenericTxApi_.bind(MarketNFTApi, 'buy-nft-tx')
            );
            //----------------------------
            if (result === false) {
                throw 'There was an error in the transaction';
            }
            //----------------------------
            setTxMessage('Transaction has been confirmed. Refreshing data...');
            //----------------------------
            refreshList();
            //----------------------------
            setTxMessage('Transaction has been confirmed. Data has been refreshed.');
            setTxConfirmed(result);
            //----------------------------
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxBuy(false);
    };
    //--------------------------------------
    const handleBtnWithdrawTx = async (item: MarketNFTEntityWithMetadata) => {
        //----------------------------
        if (walletStore.isConnected !== true) return;
        if (marketAddress === undefined) return;
        if (marketScript === undefined) return;
        if (mintingPolicyIDScript === undefined) return;
        if (mintingPolicyID_CS === undefined) return;
        //----------------------------
        setIsTxModalOpen(true);
        //----------------------------
        if (isLoadingTxWithdraw) {
            return;
        }
        //----------------------------
        setIsLoadingTxWithdraw(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(item.entity.sellingToken_TN);
        const token_CS = item.entity.sellingToken_CS;
        try {
            //----------------------------
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            //----------------------------
            const txParams: WithdrawMarketNFTTxParams = {
                marketNft_id: item.entity._DB_id,
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
            };
            //--------------------------------------
            const result = await BaseSmartDBFrontEndBtnHandlers.handleBtnDoTransactionV1(
                MarketNFTEntity,
                'Withdraw MarketNFT...',
                'Withdraw Tx',
                setTxMessage,
                setTxHash,
                walletStore,
                txParams,
                MarketNFTApi.callGenericTxApi_.bind(MarketNFTApi, 'withdraw-nft-tx')
            );
            //----------------------------
            if (result === false) {
                throw 'There was an error in the transaction';
            }
            //----------------------------
            setTxMessage('Transaction has been confirmed. Refreshing data...');
            //----------------------------
            refreshList();
            //----------------------------
            setTxMessage('Transaction has been confirmed. Data has been refreshed.');
            setTxConfirmed(result);
            //----------------------------
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxWithdraw(false);
    };
    //--------------------------------------
    return (
        <section className="bg-white rounded-lg shadow-md p-6">
            <>
                <section className="bg-white rounded-lg shadow-md p-6 mb-6">
                    <div className="text-lg font-semibold text-gray-800 mb-4">Buy Token</div>
                    {isLoading ||
                        (isLoadingList && (
                            <>
                                <LoaderButton />
                            </>
                        ))}
                </section>
                {isLoadedList === true && list !== undefined ? (
                    <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
                        {list.map((tokenToBuy, index) => (
                            <div key={index} className="bg-zinc-800 rounded-lg p-4 flex flex-col items-center text-white shadow-lg">
                                <div className="w-24 h-24 mb-4">
                                    {isValidUrl(tokenToBuy.metadata.image) ? (
                                        <img
                                            src={getUrlForImage(tokenToBuy.metadata.image!)}
                                            alt={`logo-${hexToStr(tokenToBuy.metadata.TN_Hex)}`}
                                            className="w-full h-full object-contain rounded-full"
                                        />
                                    ) : (
                                        <img
                                            src={TOKEN_ICON_GENERIC.toString()}
                                            alt={formatTokenNameHexToStr(tokenToBuy.metadata.TN_Hex)}
                                            className="w-full h-full object-contain rounded-full"
                                        />
                                    )}
                                </div>
                                <p className="text-center text-lg font-semibold mb-2">{formatTokenNameHexToStr(tokenToBuy.metadata.TN_Hex)}</p>
                                <div className="flex items-center w-full mt-2">
                                    {tokenToBuy.entity.priceOfAsset.toString()} ADA
                                    {}
                                    {walletStore.info?.pkh === tokenToBuy.entity.sellerPaymentPKH ? (
                                        <>
                                            <button
                                                className="bg-green-600 text-white font-semibold py-1 px-4 rounded-lg ml-2 transition hover:bg-green-700 active:scale-95"
                                                onClick={() => handleBtnWithdrawTx(tokenToBuy)}
                                            >
                                                Withdraw{' '}
                                                {isLoadingTxWithdraw && (
                                                    <>
                                                        <LoaderButton />
                                                    </>
                                                )}
                                            </button>
                                        </>
                                    ) : (
                                        <button
                                            className="bg-blue-600 text-white font-semibold py-1 px-4 rounded-lg ml-2 transition hover:bg-blue-700 active:scale-95"
                                            onClick={() => handleBtnBuyTx(tokenToBuy)}
                                        >
                                            Buy{' '}
                                            {isLoadingTxBuy && (
                                                <>
                                                    <LoaderButton />
                                                </>
                                            )}
                                        </button>
                                    )}
                                </div>
                            </div>
                        ))}
                    </div>
                ) : null}
                <section className="bg-white rounded-lg shadow-md p-6 mb-6">
                    <div className="text-lg font-semibold text-gray-800 mb-4">Sync DB with Blockchain</div>
                    <button
                        onClick={handleBtnSync}
                        className="bg-green-500 hover:bg-green-600 text-white font-semibold py-2 px-4 rounded transition-colors duration-200"
                        disabled={isLoadingSync}
                    >
                        Sync{' '}
                        {isLoadingSync && (
                            <>
                                <LoaderButton />
                            </>
                        )}
                    </button>
                </section>
                <Modal
                    isOpen={isTxModalOpen}
                    onRequestClose={() => setIsTxModalOpen(false)}
                    contentLabel="Transaction Status"
                    className="bg-white p-6 rounded-lg shadow-lg max-w-lg w-full"
                    overlayClassName="fixed inset-0 bg-gray-800 bg-opacity-75 flex items-center justify-center"
                >
                    <h2 className="text-xl font-semibold text-gray-800">Transaction Status</h2>
                    <div className="mt-4">
                        <textarea value={txMessage} readOnly className="w-full h-24 border border-gray-300 rounded p-2 resize-none text-gray-700"></textarea>
                    </div>
                    <div className="mt-4 flex items-center">
                        <span className="text-gray-700 font-semibold mr-2">Tx Hash:</span>
                        <div className="bg-gray-200 p-2 rounded text-gray-800">{txHash}</div>
                    </div>
                    <div className="mt-2 flex items-center">
                        <span className="text-gray-700 font-semibold mr-2">Status:</span>
                        <div
                            className={`
        ${txConfirmed ? 'text-green-500' : isTxError ? 'text-red-500' : 'text-yellow-500'}
        font-semibold
      `}
                        >
                            {txConfirmed ? 'Confirmed' : isTxError ? 'Error' : 'Waiting...'}
                        </div>
                    </div>
                    <button
                        onClick={() => setIsTxModalOpen(false)}
                        className="bg-blue-500 hover:bg-blue-600 text-white font-semibold py-2 px-4 rounded mt-4 transition-colors duration-200"
                    >
                        Close
                    </button>
                </Modal>
            </>
        </section>
    );
};
