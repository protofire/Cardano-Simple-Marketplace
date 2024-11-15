// TokenBuy.tsx
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton'; // Importing the LoaderButton component for showing loading state
import { lovelaceToAda } from '@example/src/lib/Commons/Constants/onchain'; // Importing the lovelace to ADA conversion utility
import React from 'react'; // Importing React
import ModalTransaction from '../../ModalTransaction/ModalTransaction'; // Importing ModalTransaction component for displaying transaction details
import styles from './TokenBuy.module.scss'; // Importing styles for the TokenBuy component
import { TokenBuyProps, useTokenBuy } from './useTokenBuy'; // Importing TokenBuy props and custom hook

// TokenBuy component to handle token buying and withdrawal actions
const TokenBuy: React.FC<TokenBuyProps> = ({ tokenToBuy, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    // Destructuring hook values used for handling wallet, transaction, and modal state
    const { walletStore, isLoadingTxWithdraw, isLoadingTxBuy, isTxModalOpen, txHash, isTxError, txMessage, txConfirmed, handleBtnWithdrawTx, handleBtnBuyTx, setIsTxModalOpen } =
        useTokenBuy({
            tokenToBuy, // Token data for the item to buy
            isWalletConnectorModalOpen, // State for wallet connection modal
            setIsWalletConnectorModalOpen, // Function to set wallet connection modal state
            isLoadingAnyTx, // Transaction loading state
            setIsLoadingAnyTx, // Function to set the loading state for transactions
        });

    return (
        <section>
            {/* Display token price in ADA */}
            <div className={styles.price}>
                {(Number(tokenToBuy.entity.priceOfAsset) / Number(lovelaceToAda)).toFixed(6)} ₳
            </div>

            {/* If the wallet is not connected, show the 'Connect' button */}
            {!walletStore.isConnected ? (
                <button className={styles.invalidButton} onClick={() => setIsWalletConnectorModalOpen(true)}>
                    Connect
                </button>
            ) : walletStore.info?.pkh === tokenToBuy.entity.sellerPaymentPKH ? (
                // If wallet is connected and the user is the seller, show 'Withdraw' button
                <button className={styles.withdrawButton} onClick={() => handleBtnWithdrawTx()}>
                    Withdraw
                    {isLoadingTxWithdraw && <LoaderButton />} {/* Show loader when withdrawing */}
                </button>
            ) : (
                // If the user is not the seller, show 'Buy' button
                <button className={styles.buyButton} onClick={() => handleBtnBuyTx()}>
                    Buy
                    {isLoadingTxBuy && <LoaderButton />} {/* Show loader when buying */}
                </button>
            )}

            {/* Modal displaying transaction status */}
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

export default TokenBuy; // Export TokenBuy component

