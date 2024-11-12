import { useContext, useEffect, useState } from 'react';
import { Sell } from '@/components/public/Sell/Sell';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import Modal from 'react-modal';
import { BaseSmartDBFrontEndApiCalls, CS, pushSucessNotification, pushWarningNotification, useTokensStore, useWalletStore } from 'smart-db';
import LoaderButton from '../../Commons/LoaderButton/LoaderButton';
import WalletConnector from '../../Commons/WalletConnector/WalletConnector';

import { AppStateContext, MenuClass } from '@/pages/_app';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';
import { Buy } from '../Buy/Buy';

export default function Home() {
    const walletStore = useWalletStore();
    const tokensStore = useTokensStore();
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext);
    const { menuClass, marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript, mintingPolicyID_CS } = appState;
    //----------------------------------------------------------------------------
    const [showWalletModal, setShowWalletModal] = useState(false);
    //--------------------------------------
    const [isLoading, setIsLoading] = useState(false);
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

    const handleClickMenuClass = (v: MenuClass) => {
        setAppState({
            ...appState,
            menuClass: v,
        });

        console.log(v);
    };
    return (
        <main className="pt-24 px-4 sm:px-8 lg:px-16 bg-gray-100 min-h-screen">
            {/* USER LOGGED */}

            <div className="fixed top-0 left-0 w-full bg-gray-900 text-white flex justify-between items-center p-4 shadow-md z-10">
                {/* Botones de ContractType alineados a la izquierda */}
                <div className="flex space-x-4">
                    <button
                        onClick={() => handleClickMenuClass('Buy')}
                        className={`${
                            menuClass === 'Buy' ? 'bg-gradient-to-r from-green-400 to-green-600 text-white' : 'bg-gray-700 text-gray-300 hover:bg-gray-600'
                        } font-semibold text-lg py-2 px-6 rounded-lg transition-all duration-200 ease-in-out transform active:scale-95`}
                    >
                        Buy
                    </button>
                    <button
                        onClick={() => handleClickMenuClass('Sell')}
                        className={`${
                            menuClass === 'Sell' ? 'bg-gradient-to-r from-red-400 to-red-600 text-white' : 'bg-gray-700 text-gray-300 hover:bg-gray-600'
                        } font-semibold text-lg py-2 px-6 rounded-lg transition-all duration-200 ease-in-out transform active:scale-95`}
                    >
                        Sell
                    </button>
                </div>

                {/* Selector de Wallet alineado a la derecha */}
                <div className="flex items-center space-x-2">{<WalletConnector />}</div>
            </div>

            <div className="bg-gray-100 text-gray-900 font-sans p-4">
                {menuClass === 'Buy' && <Buy />}
                {menuClass === 'Sell' && <Sell />}
            </div>
        </main>
    );
}
