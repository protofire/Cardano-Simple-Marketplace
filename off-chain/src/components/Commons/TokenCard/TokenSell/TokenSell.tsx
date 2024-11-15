
// TokenSell.tsx
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton'; // Import LoaderButton for displaying loading indicator on the sell button
import React from 'react'; // React library
import ModalTransaction from '../../ModalTransaction/ModalTransaction'; // Import ModalTransaction for handling transaction status display
import styles from './TokenSell.module.scss'; // Import styles for TokenSell component
import { TokenSellProps, useTokenSell } from './useTokenSell'; // Import TokenSellProps for prop types and useTokenSell hook for handling logic

// TokenSell component, responsible for handling the sale of tokens
const TokenSell: React.FC<TokenSellProps> = ({ tokenToSell, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    // Use the custom hook useTokenSell to manage logic related to token selling
    const {
        walletStore,
        inputValue,
        isLoadingTxSell,
        isTxModalOpen,
        txHash,
        isTxError,
        txMessage,
        txConfirmed,
        handleBtnSellTx,
        handleInputChange,
        setIsTxModalOpen,
        isValidInput,
    } = useTokenSell({
        tokenToSell,
        isWalletConnectorModalOpen,
        setIsWalletConnectorModalOpen,
        isLoadingAnyTx,
        setIsLoadingAnyTx,
    });

    return (
        <section>
            {/* Input field for entering amount to sell */}
            <div className={styles.inputContainer}>
                <input 
                    value={inputValue} 
                    onChange={handleInputChange} 
                    type="text" 
                    placeholder="Lovelace" 
                    className={styles.input} 
                />
                {/* Button to connect wallet if not already connected */}
                {!walletStore.isConnected ? (
                    <button className={styles.invalidButton} onClick={() => setIsWalletConnectorModalOpen(true)}>
                        Connect
                    </button>
                ) : (
                    // Sell button is conditionally enabled based on validity of input and transaction loading state
                    <button
                        className={
                            !isValidInput || !(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToSell.CS + tokenToSell.TN_Hex) ? styles.invalidButton : styles.sellButton
                        }
                        disabled={!isValidInput || !(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToSell.CS + tokenToSell.TN_Hex)}
                        onClick={handleBtnSellTx} // Trigger token sell transaction
                    >
                        Sell {isLoadingTxSell && <LoaderButton />} {/* Display loader while selling */}
                    </button>
                )}
            </div>

            {/* Modal to display transaction status (success, error, confirmation) */}
            <ModalTransaction
                isOpen={isTxModalOpen} // Modal open state
                onRequestClose={() => setIsTxModalOpen(false)} // Close modal on request
                txMessage={txMessage} // Message to show in the modal
                txHash={txHash!} // Transaction hash for the sale
                txConfirmed={txConfirmed} // Boolean indicating whether the transaction was confirmed
                isTxError={isTxError} // Boolean indicating if there was an error in the transaction
            />
        </section>
    );
};

export default TokenSell; // Export TokenSell component for use in other parts of the application

