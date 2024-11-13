import { useContext, useEffect } from 'react';
import { Sell } from '@/components/public/Sell/Sell';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { BaseSmartDBFrontEndApiCalls, CS, useWalletStore } from 'smart-db';
import WalletConnector from '../../Commons/WalletConnector/WalletConnector';
import styles from './Home.module.scss';

import { AppStateContext, MenuClass } from '@/pages/_app';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';
import { Buy } from '../Buy/Buy';

export default function Home() {
  const walletStore = useWalletStore();
  //----------------------------------------------------------------------------
  const { appState, setAppState } = useContext(AppStateContext);
  const { menuClass, marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript } = appState;
  //----------------------------------------------------------------------------
  
  // Function to generate scripts needed for transactions using Lucid
  async function generateScripts(lucid: Lucid) {
    let newAppState = { ...appState };

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

    // Create a hook for API calls related to the MarketNFTEntity
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
          <button 
            onClick={() => handleClickMenuClass('Buy')} 
            className={`${menuClass === 'Buy' ? styles.buy : styles.inactive} ${styles.button}`}
          >
            Buy
          </button>
          <button 
            onClick={walletStore.isConnected ? () => handleClickMenuClass('Sell') : () => {}} 
            className={`${menuClass === 'Sell' ? styles.sell : styles.inactive} ${styles.button}`}
          >
            Sell
          </button>
        </div>

        {/* Wallet selector, aligned to the right */}
        <div className={styles.walletConnector}>{<WalletConnector />}</div>
      </header>
      
      {/* Content section that conditionally renders the Buy or Sell component */}
      <div className={styles.content}>
        {menuClass === 'Buy' && <Buy />}  {/* Render Buy component if menuClass is 'Buy' */}
        {menuClass === 'Sell' && <Sell />}  {/* Render Sell component if menuClass is 'Sell' */}
      </div>
    </main>
  );
}

