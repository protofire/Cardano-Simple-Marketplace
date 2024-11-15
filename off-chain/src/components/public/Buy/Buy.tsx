
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import TokenCard from '../../Commons/TokenCard/TokenCard';
import styles from './Buy.module.scss';
import { BuyProps, useBuy } from './useBuy';

export const Buy: React.FC<BuyProps> = ({ isWalletConnectorModalOpen, setIsWalletConnectorModalOpen }) => {
    const { isLoading, isLoadingSync, isLoadingList, isLoadedList, list, isLoadingAnyTx, setIsLoadingAnyTx, handleBtnSync } = useBuy({
        isWalletConnectorModalOpen,
        setIsWalletConnectorModalOpen,
    });

    // JSX layout of the component
    return (
        <section className={styles.section}>
            <>
                <section className={styles.section}>
                    <div className={styles.header}>Sync DB with Blockchain</div>
                    <button onClick={handleBtnSync} className={styles.syncButton} disabled={isLoadingSync}>
                        Sync
                        {isLoadingSync && <LoaderButton />} {/* Show loader when syncing */}
                    </button>
                </section>
                <div className={styles.header}>Buy Token {isLoading || (isLoadingList && <LoaderButton />)}</div>
                {isLoadedList && list !== undefined ? (
                    <div className={styles.tokenGrid}>
                        {list.map((tokenToBuy, index) => (
                            <div key={index}>
                                <TokenCard
                                    tokenToBuy={tokenToBuy}
                                    sell={false}
                                    isWalletConnectorModalOpen={isWalletConnectorModalOpen}
                                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen}
                                    isLoadingAnyTx={isLoadingAnyTx}
                                    setIsLoadingAnyTx={setIsLoadingAnyTx}
                                />
                            </div>
                        ))}
                    </div>
                ) : null}
            </>
        </section>
    );
};
