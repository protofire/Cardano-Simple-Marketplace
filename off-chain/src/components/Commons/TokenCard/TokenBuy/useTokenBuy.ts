import { mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { BuyMarketNFTTxParams, WithdrawMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd';
import { AppStateContext } from '@example/src/pages/_app';
import { Credential } from 'lucid-cardano';
import React, { useContext, useState } from 'react';
import { BaseSmartDBFrontEndBtnHandlers, hexToStr, TokenMetadataEntity, useWalletStore } from 'smart-db';

export type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

export interface TokenBuyProps {
    tokenToBuy: MarketNFTEntityWithMetadata;
    isWalletConnectorModalOpen: boolean;
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>;
    isLoadingAnyTx: string | undefined; // Pass down from parent
    setIsLoadingAnyTx: React.Dispatch<React.SetStateAction<string | undefined>>; // Pass down from parent
}

export const useTokenBuy = ({ tokenToBuy, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }: TokenBuyProps) => {
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
    //----------------------------------------------------------------------------

    const handleBtnBuyTx = async () => {
        if (walletStore.isConnected !== true) return; // Ensure the wallet is connected
        if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) return;

        if (!(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToBuy.entity.sellingToken_CS + tokenToBuy.entity.sellingToken_TN)) return;

        setIsTxModalOpen(true); // Open transaction modal

        if (isLoadingTxBuy) return;

        setIsLoadingAnyTx(tokenToBuy.entity.sellingToken_CS + tokenToBuy.entity.sellingToken_TN);
        setIsLoadingTxBuy(true);

        setTxConfirmed(false);
        const token_TN = hexToStr(tokenToBuy.entity.sellingToken_TN); // Decode the token name
        const token_CS = tokenToBuy.entity.sellingToken_CS; // Token script hash
        try {
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            const paymentCredential: Credential = {
                type: 'Key',
                hash: tokenToBuy.entity.sellerPaymentPKH,
            };
            const address = (await walletStore.getLucid())!.utils.credentialToAddress(paymentCredential); // Get address from payment credential
            const txParams: BuyMarketNFTTxParams = {
                marketNft_id: tokenToBuy.entity._DB_id,
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                sellerAddress: address,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(tokenToBuy.entity.priceOfAsset), // Convert price to BigInt
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
        setIsLoadingAnyTx(undefined);
        setIsLoadingTxBuy(false);
    };

    // Handle withdrawing transaction for a MarketNFT
    const handleBtnWithdrawTx = async () => {
        if (walletStore.isConnected !== true) return; // Ensure the wallet is connected
        if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) return;

        if (!(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToBuy.entity.sellingToken_CS + tokenToBuy.entity.sellingToken_TN)) return;

        setIsTxModalOpen(true); // Open transaction modal
        if (isLoadingTxWithdraw) return;
        setIsLoadingTxWithdraw(true);
        setTxConfirmed(false);
        const token_TN = hexToStr(tokenToBuy.entity.sellingToken_TN); // Decode the token name
        const token_CS = tokenToBuy.entity.sellingToken_CS; // Token script hash
        try {
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...');
            const txParams: WithdrawMarketNFTTxParams = {
                marketNft_id: tokenToBuy.entity._DB_id,
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

    return {
        appState,
        walletStore,
        isLoadingTxBuy,
        setIsLoadingTxBuy,
        isLoadingTxWithdraw,
        setIsLoadingTxWithdraw,
        isTxModalOpen,
        setIsTxModalOpen,
        txHash,
        setTxHash,
        isTxError,
        setIsTxError,
        txMessage,
        setTxMessage,
        txConfirmed,
        setTxConfirmed,
        handleBtnBuyTx,
        handleBtnWithdrawTx,
    };
};
