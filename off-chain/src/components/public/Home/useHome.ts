// useHome.tsx
import { useContext, useEffect, useState } from 'react';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { BaseSmartDBFrontEndApiCalls, CS, useWalletStore } from 'smart-db';
import { AppStateContext, MenuClass } from '@/pages/_app';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';

export const useHome = () => {
  // This store comes from the global store provided by SmartDB.
  // It provides all necessary utilities for managing the connected wallet,
  // including its state, connection, and interaction with the blockchain.
  const walletStore = useWalletStore();
  //----------------------------------------------------------------------------

  // Access the global application state and state updater function from the AppStateContext.
  // These properties allow managing and sharing app-wide states such as `menuClass`, `marketScript`,
  // `marketAddress`, and the minting policy configurations across components.
  const { appState, setAppState } = useContext(AppStateContext);
  const { menuClass, marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript } = appState;
  //----------------------------------------------------------------------------

  // This state is used to control the visibility of the wallet connector modal.
  // By managing this at the parent component level, it allows child components
  // to trigger or access the modal state as needed.
  const [isWalletConnectorModalOpen, setIsWalletConnectorModalOpen] = useState(false);
  //----------------------------------------------------------------------------

  // Function to generate scripts required for transactions using the Lucid library.
  async function generateScripts(lucid: Lucid) {
    let newAppState = { ...appState };

    // Example implementation: The minting policy script here is not parametrized.
    // However, in a real-world scenario, the script could have been exported directly
    // from the smart contract project with parameters applied, making it ready to use.
    if (mintingPolicyIDScript === undefined || marketAddress === undefined) {
      // Apply parameters to the minting policy script.
      const mintingPolicyIDScript_: MintingPolicy = {
        type: 'PlutusV2',
        script: applyParamsToScript(mintingPolicyIDPreScript?.script, [lucid!.utils.validatorToScriptHash(marketScript)]),
      };

      // Generate the minting policy ID based on the script.
      const policyID_CS: CS = lucid.utils.mintingPolicyToId(mintingPolicyIDScript_);

      // Update the app state with the generated minting policy ID.
      newAppState = {
        mintingPolicyIDScript: mintingPolicyIDScript_,
        mintingPolicyID_CS: policyID_CS,
        ...newAppState,
      };

      console.log(`mintingPolicyID_CS: ${policyID_CS}`);

      // Generate and update the market address from the validator script.
      const marketAddress_ = lucid.utils.validatorToAddress(marketScript);
      newAppState = { marketAddress: marketAddress_, ...newAppState };

      console.log(`marketAddress: ${marketAddress_}`);

      // Update the global app state with the new configurations.
      setAppState(newAppState);
    }

    // Set up a synchronization hook for `MarketNFTEntity` with the blockchain,
    // allowing the app to stay updated with the latest data for this entity.
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
  return {
    isWalletConnectorModalOpen,
    setIsWalletConnectorModalOpen,
    menuClass,
    handleClickMenuClass,
  };
};
