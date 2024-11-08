import { useContext, useEffect, useState } from 'react';

import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { BuyMarketNFTTxParams, SellMarketNFTTxParams, WithdrawMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { applyParamsToScript, Assets, MintingPolicy, UTxO } from 'lucid-cardano';
import { useDetailsBalance } from '@/components/Commons/ModalUTxOs/DetailsBalance/useDetailsBalance';
import { Address, Lucid, Script, SpendingValidator } from 'lucid-cardano';
import {
    BaseSmartDBFrontEndApiCalls,
    BaseSmartDBFrontEndBtnHandlers,
    CS,
    formatTokenNameHexToStr,
    getUrlForImage,
    hexToStr,
    isValidUrl,
    Token,
    TOKEN_ICON_GENERIC,
    TokensWithMetadataAndAmount,
    useTokensStore,
    useWalletStore,
} from 'smart-db';
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';

import { AppStateContext, marketScript, MenuClass, mintingPolicyIDScript } from '@/pages/_app';

import Image from 'next/image';
import { Value } from 'lucid-cardano/esm/src/core/libs/cardano_multiplatform_lib/cardano_multiplatform_lib.generated';
import { console_log } from 'smart-db/backEnd';

export const Sell = ({ uTxOs }: { uTxOs: UTxO[] }) => {
    const { appStore, isRefreshing, isLoadingDetails, isLoadedDetails, current, refreshDetails } = useDetailsBalance({ uTxOs });
    const walletStore = useWalletStore();
    const tokensStore = useTokensStore();
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext);
    const { lucid, wAddr, marketAddress } = appState;
    //----------------------------------------------------------------------------
    const [isLoading, setIsLoading] = useState(false);
    const [isLoadingSync, setIsLoadingSync] = useState(false);
    const [isLoadingBalance, setIsLoadingBalance] = useState(false);
    const [isLoadingTxCreate, setIsLoadingTxCreate] = useState(false);
    const [isLoadingTxUpdate, setIsLoadingTxUpdate] = useState(false);
    //----------------------------------------------------------------------------
    const [list, setList] = useState<MarketNFTEntity[]>();
    const [selectedItem, setSelectedItem] = useState<MarketNFTEntity>();
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false);
    const [txHash, setTxHash] = useState<string>();
    const [isTxError, setIsTxError] = useState(false);
    const [txMessage, setTxMessage] = useState('');
    const [txConfirmed, setTxConfirmed] = useState(false);
    //----------------------------------------------------------------------------
    const [inputValue, setInputValue] = useState('');
    //----------------------------------------------------------------------------
    const [datumID_CS, setDatumID_CS] = useState<string>();
    const [datumID_TN, setDatumID_TN] = useState<string>();

    const mintingPolicyID: MintingPolicy = {
        type: 'PlutusV2',
        script: applyParamsToScript(mintingPolicyIDScript, [lucid!.utils.validatorToScriptHash(marketScript)]),
    };

    async function generateScripts(lucid: Lucid) {
        //----------------------------
        if (lucid === undefined) return;
        if (mintingPolicyID === undefined) return;
        if (marketScript === undefined) return;
        if (marketAddress === undefined) return;
        //----------------------------
        const policyID_CS: CS = lucid.utils.mintingPolicyToId(mintingPolicyID);
        const tokenName = 'MarketPolicyID';
        console.log(`policyID_CS: ${policyID_CS}`);
        //----------------------------
        console.log(`validatorAddress: ${marketAddress}`);
        //----------------------------
        setDatumID_CS(policyID_CS);
        setDatumID_TN(tokenName);
        //----------------------------
        await BaseSmartDBFrontEndApiCalls.createHookApi(MarketNFTEntity, marketAddress, policyID_CS);
        //----------------------------
    }

    useEffect(() => {
        const fetch = async () => {
            //----------------------------
            setIsLoading(true);
            //----------------------------
            if (lucid === undefined) return;
            try {
                await generateScripts(lucid);
                const list: MarketNFTEntity[] = await MarketNFTApi.getAllApi_({
                    fieldsForSelect: {},
                    loadRelations: { smartUTxO_id: true },
                });
                setList(list);
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
    }, []);

    const handleBtnSellTx = async (item: Token) => {
        //----------------------------
        if (lucid === undefined) return;
        if (walletStore.isConnected !== true) return;
        if (appState.wAddr === undefined) return;
        if (datumID_CS === undefined) return;
        if (datumID_TN === undefined) return;
        if (marketAddress === undefined) return;
        if (marketScript === undefined) return;
        //----------------------------
        setIsTxModalOpen(true);
        //----------------------------
        if (isLoadingTxCreate) {
            return;
        }
        //----------------------------
        setIsLoadingTxCreate(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(item.TN_Hex);
        const token_CS = item.CS;
        try {
            //----------------------------
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            //----------------------------

            console.log(lucid.utils.validatorToScriptHash(marketScript));
            console.log(lucid.utils.getAddressDetails(marketAddress).address.hex);
            console.log(mintingPolicyID);
            const txParams: SellMarketNFTTxParams = {
                token_TN,
                token_CS,
                datumID_CS,
                datumID_TN,
                validatorAddress: marketAddress,
                mintingPolicyID: mintingPolicyID,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(inputValue),
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
            const list: MarketNFTEntity[] = await MarketNFTApi.getAllApi_({
                fieldsForSelect: {},
                loadRelations: { smartUTxO_id: true },
            });
            setList(list);
            //----------------------------
            setTxMessage('Transaction has been confirmed. Data has been refreshed.');
            setTxConfirmed(result);
            //----------------------------
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxCreate(false);
    };

    return isLoadingDetails ? (
        <div className="flex justify-center items-center h-48">
            <LoaderButton />
        </div>
    ) : (
        <>
            {isLoadedDetails && current !== undefined ? (
                <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
                    {current.map((asset, index) => (
                        <div key={index} className="bg-zinc-800 rounded-lg p-4 flex flex-col items-center text-white shadow-lg">
                            <div className="w-24 h-24 mb-4">
                                {isValidUrl(asset.image) ? (
                                    <img src={getUrlForImage(asset.image!)} alt={`logo-${hexToStr(asset.TN_Hex)}`} className="w-full h-full object-contain rounded-full" />
                                ) : (
                                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(asset.TN_Hex)} className="w-full h-full object-contain rounded-full" />
                                )}
                            </div>
                            <p className="text-center text-lg font-semibold mb-2">{formatTokenNameHexToStr(asset.TN_Hex)}</p>
                            <div className="flex items-center w-full mt-2">
                                <input
                                    value={inputValue}
                                    onChange={(e) => setInputValue(e.target.value)}
                                    type="text"
                                    placeholder="Enter price"
                                    className="bg-zinc-700 text-white rounded-lg px-3 py-1 w-2/3 focus:outline-none focus:ring focus:ring-green-500"
                                />
                                <button
                                    className="bg-green-600 text-white font-semibold py-1 px-4 rounded-lg ml-2 transition hover:bg-green-700 active:scale-95"
                                    onClick={() => handleBtnSellTx(asset)}
                                >
                                    Sell
                                </button>
                            </div>
                        </div>
                    ))}
                </div>
            ) : null}
        </>
    );
};
