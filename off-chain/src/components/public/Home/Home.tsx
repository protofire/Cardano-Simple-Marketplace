import { useContext, useEffect, useState } from 'react';
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
  const { menuClass, marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript, } = appState;
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
      newAppState = {
        mintingPolicyIDScript: mintingPolicyIDScript_,
        mintingPolicyID_CS: policyID_CS,
        ...newAppState,
      };
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
      if (walletStore._lucidForUseAsUtils === undefined) return;
      try {
        await generateScripts(walletStore._lucidForUseAsUtils);
      } catch (e) {
        console.error(e);
      }
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
    <main className={styles.main}>
      {/* USER LOGGED */}
      <header className={styles.navbar}>
        {/* Botones de ContractType alineados a la izquierda */}
        <div className={styles.buttonContainer}>
          <button onClick={() => handleClickMenuClass('Buy')} className={`${menuClass === 'Buy' ? styles.buy : styles.inactive} ${styles.button}`}>
            Buy
          </button>
          <button onClick={ walletStore.isConnected ? () => handleClickMenuClass('Sell') : ()=>{}} className={`${menuClass === 'Sell' ? styles.sell : styles.inactive} ${styles.button}`}>
            Sell
          </button>
        </div>

        {/* Selector de Wallet alineado a la derecha */}
        <div className={styles.walletConnector}>{<WalletConnector />}</div>
      </header>
      <div className={styles.content}>
        {menuClass === 'Buy' && <Buy />}
        {menuClass === 'Sell' && <Sell />}
      </div>
    </main>
  );
}
