import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import { isTokenADA } from 'smart-db';
import TokenCard from '../../Commons/TokenCard/TokenCard';
import styles from './Sell.module.scss';
import { SellProps, useSell } from './useSell';

export const Sell: React.FC<SellProps> = ({ isWalletConnectorModalOpen, setIsWalletConnectorModalOpen }) => {
    //----------------------------------------------------------------------------
    // Get all the necessary state and functions from the useSell hook
    const { walletStore, isLoading, isLoadingDetails, isLoadedDetails, current, isLoadingAnyTx, setIsLoadingAnyTx } = useSell();

    return (
        <section className={styles.section}>
            {!walletStore.isConnected ? (
                <section className={styles.section}>
                    <button className={styles.connectButton} onClick={() => setIsWalletConnectorModalOpen(true)}>
                        Connect Wallet
                    </button>
                </section>
            ) : (
                <>
                    {/* Header section with loading indicators */}
                    <div className={styles.sellTokenHeader}>Sell Token {isLoading || (isLoadingDetails && <LoaderButton />)}</div>

                    {/* Display asset grid when details are loaded */}
                    {isLoadedDetails  ? (
                        <div className={styles.assetGrid}>
                            {current !== undefined && current.length > 0 ? (
                                current.map((asset, index) =>
                                    // Skip ADA tokens and only show other assets
                                    isTokenADA(asset.CS, asset.TN_Hex) ? null : (
                                        <div key={index} className={styles.assetCard}>
                                            <TokenCard
                                                tokenToSell={asset}
                                                sell={true}
                                                isWalletConnectorModalOpen={isWalletConnectorModalOpen}
                                                setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen}
                                                isLoadingAnyTx={isLoadingAnyTx}
                                                setIsLoadingAnyTx={setIsLoadingAnyTx}
                                            />
                                        </div>
                                    )
                                )
                            ) : (
                                <>You dont have tokens to sell</>
                            )}
                        </div>
                    ) : null}
                </>
            )}
        </section>
    );
};
