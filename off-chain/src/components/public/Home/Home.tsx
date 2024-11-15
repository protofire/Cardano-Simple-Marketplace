import { useContext, useEffect, useState } from 'react';
import { Sell } from '@/components/public/Sell/Sell';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { BaseSmartDBFrontEndApiCalls, CS, pushWarningNotification, useWalletStore } from 'smart-db';
import WalletConnector from '../../Commons/WalletConnector/WalletConnector';
import styles from './Home.module.scss';

import { AppStateContext, MenuClass } from '@/pages/_app';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';
import { Buy } from '../Buy/Buy';

//TODO: mover todo el codigo a useHome, fijate los useSell of useTokenSell

export default function Home() {
    //TODO tratar de explicar los Store que vienen del global store de SmartDB, como este, que provee todo lo necesario para manejar la wallet conectada

    const walletStore = useWalletStore();
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext);
    const { menuClass, marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript } = appState;
    //----------------------------------------------------------------------------

    //TODO: agregar coments de esto, es para manejar desde este componente padre el estado del modal de la billetera, y qe los hijos puedan abrilo tambien
    const [isWalletConnectorModalOpen, setIsWalletConnectorModalOpen] = useState(false);
    //----------------------------------------------------------------------------

    // Function to generate scripts needed for transactions using Lucid
    async function generateScripts(lucid: Lucid) {
        let newAppState = { ...appState };

        //TODO: explicar que a modo de ejemplo se dejo la poliza sin aplicar parametros, pero que podria haber sido exportada del projecto de los contratos directamente prametrizada y lsita para usar
        // If minting policy ID or market address is not defined, generate them
        if (mintingPolicyIDScript === undefined || marketAddress === undefined) {
            // Create a minting policy script by applying parameters to the script
            const mintingPolicyIDScript_: MintingPolicy = {
                type: 'PlutusV2',
                script: applyParamsToScript(mintingPolicyIDPreScript?.script, [lucid!.utils.validatorToScriptHash(marketScript)]),
            };

            // Generate a minting policy ID from the script
            const policyID_CS: CS = lucid.utils.mintingPolicyToId(mintingPolicyIDScript_);

            // Update the app state with the generated minting policy and market address
            newAppState = {
                mintingPolicyIDScript: mintingPolicyIDScript_,
                mintingPolicyID_CS: policyID_CS,
                ...newAppState,
            };

            console.log(`mintingPolicyID_CS: ${policyID_CS}`);

            // Derive the market address from the validator script and update app state
            const marketAddress_ = lucid.utils.validatorToAddress(marketScript);
            newAppState = { marketAddress: marketAddress_, ...newAppState };

            console.log(`marketAddress: ${marketAddress_}`);

            // Update the global app state
            setAppState(newAppState);
        }

        // Create a hook for sync MarketNFTEntity with the blockchain
        await BaseSmartDBFrontEndApiCalls.createHookApi(MarketNFTEntity, newAppState.marketAddress!, newAppState.mintingPolicyID_CS!);
    }

    // Effect hook to trigger the script generation when the Lucid instance is available
    useEffect(() => {
        const fetch = async () => {
            if (walletStore._lucidForUseAsUtils === undefined) return;
            try {
                await generateScripts(walletStore._lucidForUseAsUtils); // Generate scripts with Lucid instance
            } catch (e) {
                console.error(e);
            }
        };

        fetch();
    }, [walletStore._lucidForUseAsUtils]);

    // Function to handle menu item clicks (Buy or Sell)
    const handleClickMenuClass = (v: MenuClass) => {
        // Update the app state with the selected menu class (Buy or Sell)
        setAppState({
            ...appState,
            menuClass: v,
        });

        console.log(v);
    };

    return (
        <main className={styles.main}>
            {/* USER LOGGED IN */}
            <header className={styles.navbar}>
                {/* Buttons for Buy and Sell, aligned to the left */}
                <div className={styles.buttonContainer}>
                    <button onClick={() => handleClickMenuClass('Buy')} className={`${menuClass === 'Buy' ? styles.buy : styles.inactive} ${styles.button}`}>
                        Buy
                    </button>
                    <button onClick={() => handleClickMenuClass('Sell')} className={`${menuClass === 'Sell' ? styles.buy : styles.inactive} ${styles.button}`}>
                        Sell
                    </button>
                </div>

                {/* Wallet selector, aligned to the right */}
                <div className={styles.walletConnector}>
                    {<WalletConnector isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}
                </div>
            </header>

            {/* Content section that conditionally renders the Buy or Sell component */}
            <div className={styles.content}>
                {menuClass === 'Buy' && <Buy isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}{' '}
                {/* Render Buy component if menuClass is 'Buy' */}
                {menuClass === 'Sell' && <Sell isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}{' '}
                {/* Render Sell component if menuClass is 'Sell' */}
            </div>
        </main>
    );
}
