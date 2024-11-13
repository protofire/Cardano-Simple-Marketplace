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
  const { isLoadingDetails, isLoadedDetails, current } = useDetails<TokensWithMetadataAndAmount>({
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
    setIsLoadingTxSells((prev) => ({
      ...prev,
      [item.CS + item.TN_Hex]: false,
    }));
  };
  //--------------------------------------
  // Actualizar el valor del input basado en el ID del token
  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>, tokenId: string) => {
    setInputValues((prev) => ({ ...prev, [tokenId]: e.target.value }));
  };
  //--------------------------------------

  return (
    <section className={styles.section}>
      <>
        <div className={styles.sellTokenHeader}>
          Sell Token {isLoading || isLoadingTxAnySell || (isLoadingDetails && <LoaderButton />)}
        </div>
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

      </>
    </section>
  );
};
