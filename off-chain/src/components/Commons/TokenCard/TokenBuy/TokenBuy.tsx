// TokenBuy.tsx
import React, { useContext, useState } from 'react';
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import styles from './TokenBuy.module.scss';

import {
    BaseSmartDBFrontEndBtnHandlers,
    hexToStr,
    pushWarningNotification,
    TokenMetadataEntity,
    useWalletStore,
} from 'smart-db';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import { AppStateContext } from '@example/src/pages/_app';
import { BuyMarketNFTTxParams, WithdrawMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { lovelaceToAda, mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd';
import { Credential } from 'lucid-cardano';
import ModalTransaction from '../../ModalTransaction/ModalTransaction';

type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

interface TokenBuyProps {
    tokenToBuy: MarketNFTEntityWithMetadata;
    walletConnected: boolean;
    isOwnerToken: boolean;
}

const TokenBuy: React.FC<TokenBuyProps> = ({ tokenToBuy, walletConnected, isOwnerToken }) => {
    const walletStore = useWalletStore(); // Access the wallet store
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext); // Access app state
    const { marketScript, marketAddress, mintingPolicyIDScript, mintingPolicyID_CS } = appState; // Extract relevant data from app state
    //----------------------------------------------------------------------------
    // State variables to manage loading states and transaction modal
    const [isLoadingTxBuy, setIsLoadingTxBuy] = useState(false);
    const [isLoadingTxWithdraw, setIsLoadingTxWithdraw] = useState(false);
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false); // State for showing transaction modal
    const [txHash, setTxHash] = useState<string>(); // Transaction hash state
    const [isTxError, setIsTxError] = useState(false); // Error state for transaction
    const [txMessage, setTxMessage] = useState(''); // Message related to transaction
    const [txConfirmed, setTxConfirmed] = useState(false); // Confirmation state for transaction

    const handleBtnBuyTx = async (item: MarketNFTEntityWithMetadata) => {
        if (walletStore.isConnected !== true) return; // Ensure the wallet is connected
        if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) return;
        setIsTxModalOpen(true); // Open transaction modal
        if (isLoadingTxBuy) return;
        setIsLoadingTxBuy(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(item.entity.sellingToken_TN); // Decode the token name
        const token_CS = item.entity.sellingToken_CS; // Token script hash
        try {
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            const paymentCredential: Credential = {
                type: 'Key',
                hash: item.entity.sellerPaymentPKH,
            };
            const address = (await walletStore.getLucid())!.utils.credentialToAddress(paymentCredential); // Get address from payment credential
            const txParams: BuyMarketNFTTxParams = {
                marketNft_id: item.entity._DB_id,
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                sellerAddress: address,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(item.entity.priceOfAsset), // Convert price to BigInt
            };
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
            if (result === false) {
                throw 'There was an error in the transaction';
            }
            setTxConfirmed(result);
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxBuy(false);
    };

    // Handle withdrawing transaction for a MarketNFT
    const handleBtnWithdrawTx = async (item: MarketNFTEntityWithMetadata) => {
        if (walletStore.isConnected !== true) return; // Ensure the wallet is connected
        if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) return;
        setIsTxModalOpen(true); // Open transaction modal
        if (isLoadingTxWithdraw) return;
        setIsLoadingTxWithdraw(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(item.entity.sellingToken_TN); // Decode the token name
        const token_CS = item.entity.sellingToken_CS; // Token script hash
        try {
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            const txParams: WithdrawMarketNFTTxParams = {
                marketNft_id: item.entity._DB_id,
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
            };
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
            if (result === false) {
                throw 'There was an error in the transaction';
            }
            setTxConfirmed(result);
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true);
        }
        setIsLoadingTxWithdraw(false);
    };

    return (
        <section>
            <div className={styles.price}>{(Number(tokenToBuy.entity.priceOfAsset) / Number(lovelaceToAda)).toFixed(6)} ₳</div>
            {!walletConnected ? (
                <button className={styles.invalidButton} onClick={() => pushWarningNotification('Menu', 'Connect your wallet')}></button>
            ) : isOwnerToken ? (
                <button className={styles.withdrawButton} onClick={() => handleBtnWithdrawTx(tokenToBuy)}>
                    Withdraw
                    {isLoadingTxWithdraw && <LoaderButton />} {/* Show loader when withdrawing */}
                </button>
            ) : (
                <button className={styles.buyButton} onClick={() => handleBtnBuyTx(tokenToBuy)}>
                    Buy
                    {isLoadingTxBuy && <LoaderButton />} {/* Show loader when buying */}
                </button>
            )}
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

export default TokenBuy;
