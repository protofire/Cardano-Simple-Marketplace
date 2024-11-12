import { useContext, useEffect, useState } from 'react';

import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import { AppStateContext } from '@/pages/_app';
import { mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { SellMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';
import Modal from 'react-modal';
import {
    BaseSmartDBFrontEndApiCalls,
    BaseSmartDBFrontEndBtnHandlers,
    CS,
    formatTokenNameHexToStr,
    getAssetOfUTxOs,
    getUrlForImage,
    hexToStr,
    isValidUrl,
    Token,
    TOKEN_ICON_GENERIC,
    TokenMetadataFrontEndApiCalls,
    TokensWithMetadataAndAmount,
    useDetails,
    useWalletStore,
    isTokenADA,
} from 'smart-db';
import { fa } from '@faker-js/faker/.';
//import styles from './Sell.module.scss';

export const Sell = () => {
    //----------------------------------------------------------------------------
    const walletStore = useWalletStore();
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext);
    const { marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript, mintingPolicyID_CS } = appState;
    //----------------------------------------------------------------------------
    const [isLoading, setIsLoading] = useState(false);
    //----------------------------------------------------------------------------
    // Estado para manejar input y loading por token
    const [inputValues, setInputValues] = useState<{ [key: string]: string }>({});
    const [isLoadingTxAnySell, setIsLoadingTxAnySell] = useState(false);
    const [isLoadingTxSells, setIsLoadingTxSells] = useState<{ [key: string]: boolean }>({});
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false);
    const [txHash, setTxHash] = useState<string>();
    const [isTxError, setIsTxError] = useState(false);
    const [txMessage, setTxMessage] = useState('');
    const [txConfirmed, setTxConfirmed] = useState(false);
    //----------------------------------------------------------------------------
    async function generateScripts(lucid: Lucid) {
        //----------------------------
        let newAppState = { ...appState };
        //----------------------------
        if (mintingPolicyIDScript === undefined || marketAddress === undefined) {
            const mintingPolicyIDScript_: MintingPolicy = {
                type: 'PlutusV2',
                script: applyParamsToScript(mintingPolicyIDPreScript?.script, [lucid!.utils.validatorToScriptHash(marketScript)]),
            };
            const policyID_CS: CS = lucid.utils.mintingPolicyToId(mintingPolicyIDScript_);
            //----------------------------
            newAppState = { mintingPolicyIDScript: mintingPolicyIDScript_, mintingPolicyID_CS: policyID_CS, ...newAppState };
            //----------------------------
            console.log(`mintingPolicyID_CS: ${policyID_CS}`);
            //----------------------------
            const marketAddress_ = lucid.utils.validatorToAddress(marketScript);
            newAppState = { marketAddress: marketAddress_, ...newAppState };
            console.log(`marketAddress: ${marketAddress_}`);
            setAppState(newAppState);
        }
        //----------------------------
        await BaseSmartDBFrontEndApiCalls.createHookApi(MarketNFTEntity, newAppState.marketAddress!, newAppState.mintingPolicyID_CS!);
        //----------------------------
    }
    //----------------------------------------------------------------------------
    useEffect(() => {
        const fetch = async () => {
            //----------------------------
            setIsLoading(true);
            //----------------------------
            if (walletStore._lucidForUseAsUtils === undefined) return;
            try {
                await generateScripts(walletStore._lucidForUseAsUtils);
            } catch (e) {
                console.error(e);
            }
            //----------------------------
            setIsLoading(false);
            //----------------------------
        };
        //----------------------------
        fetch();
        //----------------------------
    }, [walletStore._lucidForUseAsUtils]);
    //----------------------------------------------------------------------------
    const loadDetails = async () => {
        if (walletStore.isWalletDataLoaded === true) {
            const totalAssets = getAssetOfUTxOs(walletStore.getUTxOsAtWallet());
            const assetDetails = await TokenMetadataFrontEndApiCalls.getAssetsWithDetailsApi(totalAssets);
            return assetDetails;
        } else {
            return undefined;
        }
    };
    //--------------------------------------
    const { isLoadingDetails, isLoadedDetails, current, refreshDetails } = useDetails<TokensWithMetadataAndAmount>({
        nameDetails: 'Balance',
        loadDetails,
        dependencies: [walletStore.isWalletDataLoaded],
    });
    //--------------------------------------
    const handleBtnSellTx = async (item: Token) => {
        //----------------------------
        if (walletStore.isConnected !== true) return;
        if (marketAddress === undefined) return;
        if (marketScript === undefined) return;
        if (mintingPolicyIDScript === undefined) return;
        if (mintingPolicyID_CS === undefined) return;
        //----------------------------
        setIsTxModalOpen(true);
        //----------------------------
        if (isLoadingTxAnySell) {
            return;
        }
        //----------------------------
        setIsLoadingTxAnySell(true);
        setIsLoadingTxSells((prev) => ({ ...prev, [item.CS + item.TN_Hex]: true }));
        //----------------------------
        setTxConfirmed(false);
        const token_TN = hexToStr(item.TN_Hex);
        const token_CS = item.CS;
        try {
            //----------------------------
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            //----------------------------

            const txParams: SellMarketNFTTxParams = {
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                validatorAddress: marketAddress,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(inputValues[item.CS + item.TN_Hex]),
            };
            //--------------------------------------
            const result = await BaseSmartDBFrontEndBtnHandlers.handleBtnDoTransactionV1(
                MarketNFTEntity,
                'Creating MarketNFT...',
                'Create Tx',
                setTxMessage,
                setTxHash,
                walletStore,
                txParams,
                MarketNFTApi.callGenericTxApi_.bind(MarketNFTApi, 'sell-nft-tx')
            );
            //----------------------------
            if (result === false) {
                throw 'There was an error in the transaction';
            }
            //----------------------------
            setTxMessage('Transaction has been confirmed. Refreshing data...');
            //----------------------------
            setTxMessage('Transaction has been confirmed. Data has been refreshed.');
            setTxConfirmed(result);
            //----------------------------
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxAnySell(false);
        setIsLoadingTxSells((prev) => ({ ...prev, [item.CS + item.TN_Hex]: false }));
    };
    //--------------------------------------
    // Actualizar el valor del input basado en el ID del token
    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>, tokenId: string) => {
        setInputValues((prev) => ({ ...prev, [tokenId]: e.target.value }));
    };
    //--------------------------------------
    return (
        <section className="bg-white rounded-lg shadow-md p-6">
            <>
                <section className="bg-white rounded-lg shadow-md p-6 mb-6">
                    <div className="text-lg font-semibold text-gray-800 mb-4">Sell Token</div>
                    {isLoading ||
                        isLoadingTxAnySell ||
                        (isLoadingDetails && (
                            <>
                                <LoaderButton />
                            </>
                        ))}
                </section>
                {isLoadedDetails && current !== undefined ? (
                    <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
                        {current.map((asset, index) =>
                            isTokenADA(asset.CS, asset.TN_Hex) ? null : (
                                <>
                                    <div key={index} className="bg-zinc-800 rounded-lg p-4 flex flex-col items-center text-white shadow-lg">
                                        <div className="w-24 h-24 mb-4">
                                            {isValidUrl(asset.image) ? (
                                                <img
                                                    src={getUrlForImage(asset.image!)}
                                                    alt={`logo-${hexToStr(asset.TN_Hex)}`}
                                                    className="w-full h-full object-contain rounded-full"
                                                />
                                            ) : (
                                                <img
                                                    src={TOKEN_ICON_GENERIC.toString()}
                                                    alt={formatTokenNameHexToStr(asset.TN_Hex)}
                                                    className="w-full h-full object-contain rounded-full"
                                                />
                                            )}
                                        </div>
                                        <p className="text-center text-lg font-semibold mb-2">
                                            {formatTokenNameHexToStr(asset.TN_Hex)} [{asset.amount.toString()}]
                                        </p>
                                        <div className="flex items-center w-full mt-2">
                                            <input
                                                value={inputValues[asset.CS + asset.TN_Hex] || ''}
                                                onChange={(e) => handleInputChange(e, asset.CS + asset.TN_Hex)}
                                                type="text"
                                                placeholder="Enter price"
                                                className="bg-zinc-700 text-white rounded-lg px-3 py-1 w-2/3 focus:outline-none focus:ring focus:ring-green-500"
                                            />
                                            <button
                                                className="bg-green-600 text-white font-semibold py-1 px-4 rounded-lg ml-2 transition hover:bg-green-700 active:scale-95"
                                                onClick={() => handleBtnSellTx(asset)}
                                            >
                                                Sell {isLoadingTxSells[asset.CS + asset.TN_Hex] && <LoaderButton />}
                                            </button>
                                        </div>
                                    </div>
                                </>
                            )
                        )}
                    </div>
                ) : null}
                {/* <Modal
                isOpen={isTxModalOpen}
                onRequestClose={() => setIsTxModalOpen(false)}
                contentLabel="Transaction Status"
                className={styles.modal}
                overlayClassName={styles.overlay}
            >
                <h2>Transaction Status</h2>
                <div>
                    <textarea value={txMessage} style={{ resize: 'none' }} readOnly={true}></textarea>
                </div>
                <div>
                    Tx Hash: <div className={styles.txHash}>{txHash}</div>
                </div>
                <div>
                    Status: <div className={styles.txStatus}>{txConfirmed ? 'Confirmed' : isTxError ? 'Error' : 'Waiting...'}</div>
                </div>
                <button onClick={() => setIsTxModalOpen(false)}>Close</button>
            </Modal> */}

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
            </>{' '}
        </section>
    );
};
