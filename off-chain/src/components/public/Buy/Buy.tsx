import { useContext, useState } from 'react';

import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import ModalTransaction from '@/components/Commons/ModalTransaction/ModalTransaction';
import { AppStateContext } from '@/pages/_app';
import { lovelaceToAda, mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
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

import styles from './Buy.module.scss';

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
      const paymentCredential: Credential = {
        type: 'Key',
        hash: item.entity.sellerPaymentPKH,
      };
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
    <section className={styles.section}>
      <>
        <section className={styles.section}>
          <div className={styles.header}>Sync DB with Blockchain</div>
          <button onClick={handleBtnSync} className={styles.syncButton} disabled={isLoadingSync}>
            Sync
            {isLoadingSync && <LoaderButton />}
          </button>
        </section>
        <div className={styles.header}>Buy Token {isLoading || (isLoadingList && <LoaderButton />)}</div>
        {isLoadedList && list !== undefined ? (
          <div className={styles.tokenGrid}>
            {list.map((tokenToBuy, index) => (
              <div key={index} className={styles.tokenCard}>
                <div className={styles.tokenImage}>
                  {isValidUrl(tokenToBuy.metadata.image) ? (
                    <img src={getUrlForImage(tokenToBuy.metadata.image!)} alt={`logo-${hexToStr(tokenToBuy.metadata.TN_Hex)}`} className={styles.tokenImage} />
                  ) : (
                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(tokenToBuy.metadata.TN_Hex)} className={styles.tokenImage} />
                  )}
                </div>
                <p className={styles.tokenName}>{formatTokenNameHexToStr(tokenToBuy.metadata.TN_Hex)}</p>
                <div className={styles.price}>{(Number(tokenToBuy.entity.priceOfAsset)/Number(lovelaceToAda)).toFixed(6)} ₳</div>
                {walletStore.info?.pkh === tokenToBuy.entity.sellerPaymentPKH ? (
                  <button className={styles.withdrawButton} onClick={() => handleBtnWithdrawTx(tokenToBuy)}>
                    Withdraw
                    {isLoadingTxWithdraw && <LoaderButton />}
                  </button>
                ) : (
                  <button className={styles.buyButton} onClick={() => handleBtnBuyTx(tokenToBuy)}>
                    Buy
                    {isLoadingTxBuy && <LoaderButton />}
                  </button>
                )}
              </div>
            ))}
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
