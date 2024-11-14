// TokenSell.tsx
import React, { useContext, useState } from 'react';
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import styles from './TokenSell.module.scss';

import {
    BaseSmartDBFrontEndBtnHandlers,
    hexToStr,
    Token,
    Token_With_Metadata_And_Amount,
    useWalletStore,
} from 'smart-db';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import { AppStateContext } from '@example/src/pages/_app';
import { SellMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import {  mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd';
import ModalTransaction from '../../ModalTransaction/ModalTransaction';

interface TokenSellProps {
    tokenToSell: Token_With_Metadata_And_Amount;
}

const TokenSell: React.FC<TokenSellProps> = ({ tokenToSell: asset}) => {
    const walletStore = useWalletStore(); // Access the wallet store
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext); // Access app state
    const { marketScript, marketAddress, mintingPolicyIDScript, mintingPolicyID_CS } = appState; // Extract relevant data from app state
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false); // State for showing transaction modal
    const [txHash, setTxHash] = useState<string>(); // Transaction hash state
    const [isTxError, setIsTxError] = useState(false); // Error state for transaction
    const [txMessage, setTxMessage] = useState(''); // Message related to transaction
    const [txConfirmed, setTxConfirmed] = useState(false); // Confirmation state for transaction

    const [inputValues, setInputValues] = useState<{ [key: string]: string }>({}); // Store input values for token prices
    const [isLoadingTxAnySell, setIsLoadingTxAnySell] = useState(false); // Tracks loading state of any selling transaction
    const [isLoadingTxSells, setIsLoadingTxSells] = useState<{ [key: string]: boolean }>({}); // Tracks loading state for individual token sales

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

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>, tokenId: string) => {
        setInputValues((prev) => ({ ...prev, [tokenId]: e.target.value }));
    };

    return (
        <section>
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

export default TokenSell;
