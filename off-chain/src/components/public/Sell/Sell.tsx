
import { useContext, useEffect, useState } from 'react';
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import { AppStateContext } from '@/pages/_app';
import { mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { SellMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { applyParamsToScript, Lucid, MintingPolicy } from 'lucid-cardano';

import {
  BaseSmartDBFrontEndApiCalls,
  BaseSmartDBFrontEndBtnHandlers,
  CS,
  formatTokenNameHexToStr,
  getAssetOfUTxOs,
  getUrlForImage,
  hexToStr,
  isTokenADA,
  isValidUrl,
  Token,
  TOKEN_ICON_GENERIC,
  TokenMetadataFrontEndApiCalls,
  TokensWithMetadataAndAmount,
  useDetails,
  useWalletStore,
} from 'smart-db';
import styles from './Sell.module.scss';
import ModalTransaction from '../../Commons/ModalTransaction/ModalTransaction';

export const Sell = () => {
  //----------------------------------------------------------------------------
  // Get the wallet store state, which contains wallet-related data and actions
  const walletStore = useWalletStore();
  //----------------------------------------------------------------------------
  // Get app-level state and function to update it from the context
  const { appState, setAppState } = useContext(AppStateContext);
  const { marketScript, marketAddress, mintingPolicyIDPreScript, mintingPolicyIDScript, mintingPolicyID_CS } = appState;
  //----------------------------------------------------------------------------
  // State for managing loading and input values for tokens
  const [isLoading, setIsLoading] = useState(false); // Whether the app is loading data
  const [inputValues, setInputValues] = useState<{ [key: string]: string }>({}); // Store input values for token prices
  const [isLoadingTxAnySell, setIsLoadingTxAnySell] = useState(false); // Tracks loading state of any selling transaction
  const [isLoadingTxSells, setIsLoadingTxSells] = useState<{ [key: string]: boolean }>({}); // Tracks loading state for individual token sales
  //----------------------------------------------------------------------------
  // States for managing transaction modal and transaction details
  const [isTxModalOpen, setIsTxModalOpen] = useState(false); // Whether the transaction modal is open
  const [txHash, setTxHash] = useState<string>(); // Transaction hash
  const [isTxError, setIsTxError] = useState(false); // Whether there was an error with the transaction
  const [txMessage, setTxMessage] = useState(''); // Message to display in the modal (e.g., transaction status)
  const [txConfirmed, setTxConfirmed] = useState(false); // Whether the transaction was confirmed
  //----------------------------------------------------------------------------
  
  // Function to generate the minting script and related details if they aren't available
  async function generateScripts(lucid: Lucid) {
    let newAppState = { ...appState };

    // Check if the necessary scripts or addresses are undefined
    if (mintingPolicyIDScript === undefined || marketAddress === undefined) {
      const mintingPolicyIDScript_: MintingPolicy = {
        type: 'PlutusV2', // Define the type of minting policy (PlutusV2)
        script: applyParamsToScript(mintingPolicyIDPreScript?.script, [lucid!.utils.validatorToScriptHash(marketScript)]),
      };
      const policyID_CS: CS = lucid.utils.mintingPolicyToId(mintingPolicyIDScript_);
      newAppState = {
        mintingPolicyIDScript: mintingPolicyIDScript_,
        mintingPolicyID_CS: policyID_CS,
        ...newAppState,
      };

      console.log(`mintingPolicyID_CS: ${policyID_CS}`);

      // Generate market address from the market script
      const marketAddress_ = lucid.utils.validatorToAddress(marketScript);
      newAppState = { marketAddress: marketAddress_, ...newAppState };
      console.log(`marketAddress: ${marketAddress_}`);
      
      // Update app state with the new details
      setAppState(newAppState);
    }

    // Create API hook with the new state (e.g., market address and minting policy)
    await BaseSmartDBFrontEndApiCalls.createHookApi(MarketNFTEntity, newAppState.marketAddress!, newAppState.mintingPolicyID_CS!);
  }

  // Load data and generate scripts when the component is mounted or wallet data changes
  useEffect(() => {
    const fetch = async () => {
      setIsLoading(true); // Show loader while data is being fetched
      if (walletStore._lucidForUseAsUtils === undefined) return; // Ensure Lucid is available before proceeding
      try {
        await generateScripts(walletStore._lucidForUseAsUtils); // Generate scripts based on the Lucid instance
      } catch (e) {
        console.error(e); // Handle errors if the script generation fails
      }
      setIsLoading(false); // Hide the loader once data is fetched
    };

    fetch();
  }, [walletStore._lucidForUseAsUtils]); // Effect runs when wallet's Lucid instance is available

  // Function to load the details of the user's assets
  const loadDetails = async () => {
    if (walletStore.isWalletDataLoaded === true) {
      const totalAssets = getAssetOfUTxOs(walletStore.getUTxOsAtWallet()); // Fetch wallet's UTxOs
      const assetDetails = await TokenMetadataFrontEndApiCalls.getAssetsWithDetailsApi(totalAssets); // Get metadata details for each asset
      return assetDetails;
    } else {
      return undefined; // Return undefined if wallet data isn't loaded
    }
  };

  const { isLoadingDetails, isLoadedDetails, current } = useDetails<TokensWithMetadataAndAmount>({
    nameDetails: 'Balance',
    loadDetails, // Fetches details using the loadDetails function
    dependencies: [walletStore.isWalletDataLoaded], // Dependency on wallet data being loaded
  });

  // Function to handle the sell transaction for a specific asset
  const handleBtnSellTx = async (item: Token) => {
    if (walletStore.isConnected !== true) return; // Ensure wallet is connected
    if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) {
      return; // Ensure all required values are available before proceeding
    }
    
    setIsTxModalOpen(true); // Open transaction modal

    if (isLoadingTxAnySell) return; // Prevent multiple transactions from being created simultaneously
    setIsLoadingTxAnySell(true);
    setIsLoadingTxSells((prev) => ({ ...prev, [item.CS + item.TN_Hex]: true }));

    setTxConfirmed(false);
    const token_TN = hexToStr(item.TN_Hex); // Convert token name from hex to string
    const token_CS = item.CS; // Get the token's CS value

    try {
      setTxHash(undefined);
      setIsTxError(false);
      setTxMessage('Creating Transaction...'); // Show loading message

      // Set up parameters for the transaction
      const txParams: SellMarketNFTTxParams = {
        token_TN,
        token_CS,
        datumID_CS: mintingPolicyID_CS,
        datumID_TN: mintingPolicyID_TN,
        validatorAddress: marketAddress,
        mintingPolicyID: mintingPolicyIDScript,
        validatorMarket: marketScript,
        priceOfAsset: BigInt(inputValues[item.CS + item.TN_Hex]), // Use the input value as the asset price
      };

      // Call the transaction handler to process the transaction
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

      if (result === false) {
        throw 'There was an error in the transaction'; // Handle failure
      }

      setTxMessage('Transaction has been confirmed. Refreshing data...');
      setTxConfirmed(result); // Set transaction as confirmed
    } catch (e) {
      console.error(e);
      setTxHash(undefined);
      setIsTxError(true); // Set error flag if transaction fails
    }

    setIsLoadingTxAnySell(false);
    setIsLoadingTxSells((prev) => ({
      ...prev,
      [item.CS + item.TN_Hex]: false,
    }));
  };

  // Handle changes to the input fields for token prices
  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>, tokenId: string) => {
    setInputValues((prev) => ({ ...prev, [tokenId]: e.target.value }));
  };

  return (
    <section className={styles.section}>
      <div className={styles.sellTokenHeader}>
        Sell Token {isLoading || isLoadingTxAnySell || (isLoadingDetails && <LoaderButton />)}
      </div>

      {/* Check if the asset details are loaded and show them */}
      {isLoadedDetails && current !== undefined ? (
        <div className={styles.assetGrid}>
          {current.map((asset, index) =>
            isTokenADA(asset.CS, asset.TN_Hex) ? null : (
              <div key={index} className={styles.assetCard}>
                <div className={styles.assetImage}>
                  {isValidUrl(asset.image) ? (
                    <img src={getUrlForImage(asset.image!)} alt={`logo-${hexToStr(asset.TN_Hex)}`} className={styles.assetImage} />
                  ) : (
                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(asset.TN_Hex)} className={styles.assetImage} />
                  )}
                </div>
                <p className={styles.assetName}>
                  {formatTokenNameHexToStr(asset.TN_Hex)} [{asset.amount.toString()}]
                </p>
                <div className={styles.inputContainer}>
                  <input
                    value={inputValues[asset.CS + asset.TN_Hex] || ''}
                    onChange={(e) => handleInputChange(e, asset.CS + asset.TN_Hex)}
                    type="text"
                    placeholder="Lovelace"
                    className={styles.input}
                  />
                  <button className={styles.sellButton} onClick={() => handleBtnSellTx(asset)}>
                    Sell {isLoadingTxSells[asset.CS + asset.TN_Hex] && <LoaderButton />}
                  </button>
                </div>
              </div>
            )
          )}
        </div>
      ) : null}

      <ModalTransaction
        isOpen={isTxModalOpen}
        onRequestClose={() => setIsTxModalOpen(false)}
        txMessage={txMessage}
        txHash={txHash!}
        txConfirmed={txConfirmed}
        isTxError={isTxError}
      />
    </section>
  );
};

