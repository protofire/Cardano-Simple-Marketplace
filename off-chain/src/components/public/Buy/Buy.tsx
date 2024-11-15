import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton'; // Import LoaderButton component for showing loading state
import TokenCard from '../../Commons/TokenCard/TokenCard'; // Import TokenCard component to display each token's information
import styles from './Buy.module.scss'; // Import styles for the Buy component
import { BuyProps, useBuy } from './useBuy'; // Import BuyProps type and useBuy custom hook for managing component logic

export const Buy: React.FC<BuyProps> = ({ isWalletConnectorModalOpen, setIsWalletConnectorModalOpen }) => {
    // Destructure necessary values and functions from the useBuy custom hook
    const { 
        isLoading, 
        isLoadingSync, 
        isLoadingList, 
        isLoadedList, 
        list, 
        isLoadingAnyTx, 
        setIsLoadingAnyTx, 
        handleBtnSync 
    } = useBuy();

    // JSX layout of the component
    return (
        <section className={styles.section}>
            <>
                <section className={styles.section}>
                    <div className={styles.header}>Sync DB with Blockchain</div>
                    {/* Button to trigger database synchronization with blockchain, shows loader if syncing */}
                    <button onClick={handleBtnSync} className={styles.syncButton} disabled={isLoadingSync}>
                        Sync
                        {isLoadingSync && <LoaderButton />} {/* Show loader when syncing */}
                    </button>
                </section>

                {/* Header for the Buy Token section, shows loader if the list or token data is loading */}
                <div className={styles.header}>Buy Token {isLoading || (isLoadingList && <LoaderButton />)}</div>

                {/* If the list is loaded, display the tokens in a grid using TokenCard component */}
                {isLoadedList && list !== undefined ? (
                    <div className={styles.tokenGrid}>
                        {/* Map through the list of tokens and render a TokenCard for each token */}
                        {list.map((tokenToBuy, index) => (
                            <div key={index}>
                                <TokenCard
                                    tokenToBuy={tokenToBuy} // Pass token data to TokenCard
                                    sell={false} // Indicates that this is the buy action (not sell)
                                    isWalletConnectorModalOpen={isWalletConnectorModalOpen} // Pass wallet modal state
                                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} // Function to control the modal state
                                    isLoadingAnyTx={isLoadingAnyTx} // Pass loading state for any transaction
                                    setIsLoadingAnyTx={setIsLoadingAnyTx} // Function to update loading state for any transaction
                                />
                            </div>
                        ))}
                    </div>
                ) : null} {/* Show nothing if list is not loaded */}
            </>
        </section>
    );
};

