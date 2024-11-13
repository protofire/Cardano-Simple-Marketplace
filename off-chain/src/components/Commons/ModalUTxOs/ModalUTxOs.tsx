import { UTxO } from 'lucid-cardano';
import { useModalUTxOsAndBalance } from './useModalUTxOs';
import styles from './ModalUTxOs.module.scss';
import { CLOSE, formatAddress, CopyButton, OpenInNewTabButton } from 'smart-db';
import LoaderButton from '../LoaderButton/LoaderButton';
import { DetailsBalance } from './DetailsBalance/DetailsBalance';
import { DetailsUTxOs } from './DetailsUTxO/DetailsUTxO';
import Image from 'next/image';

export const ModalUTxOsAndBalance = ({ address, uTxOs, showBalance = false }: { address: string; uTxOs?: UTxO[]; showBalance?: boolean }) => {
    //--------------------------------------
    // State and functions from `useModalUTxOsAndBalance`
    const { appStore, isRefreshing, isLoadingList, list, showBalance_, isOpen, handleOpen, handleClose, setShowBalance } = useModalUTxOsAndBalance({
        address,
        uTxOs,
        showBalance,
    });
    //--------------------------------------

    // Button to open the modal, showing either 'Balance' or 'UTxO(s)' based on `showBalance` and `list` length
    return (
        <>
            <button className={styles.walletDetails} onClick={handleOpen}>
                View {showBalance ? 'Balance' : list.length <= 1 ? 'UTxO' : 'UTxOs'}
            </button>

            {isOpen ? (
                <div className={styles.modal}>
                    <div className={`${styles.main}`}>
                        <div className={styles.popUp}>
                            {/* Header of Modal */}
                            <header className={styles.headerModal}>
                                <h2 className={styles.titleModal}>{showBalance_ ? 'Balance' : list.length <= 1 ? 'UTxO' : 'UTxOs'}</h2>
                                
                                {/* Toggle button to switch between Balance and UTxOs views */}
                                <div className={styles.buttonsModal}>
                                    <button
                                        onClick={() => {
                                            setShowBalance(!showBalance_);
                                        }}
                                    >
                                        {showBalance_ ? (list.length <= 1 ? 'Show UTxO' : 'Show UTxOs') : 'Show Balance'}
                                    </button>
                                </div>

                                {/* Close button */}
                                <button className={styles.closeButton} onClick={handleClose}>
                                    <Image width={24} height={24} src={CLOSE.toString()} alt="Close icon" />
                                </button>
                            </header>

                            {/* Address Display with Copy and External Link Buttons */}
                            <div className={styles.rowSettings}>
                                <div className={styles.itemWidthIcons}>
                                    <p className={styles.title}>Address:</p>
                                    <p className={styles.value}>{formatAddress(address)}</p>
                                    <CopyButton content={address} />
                                    <OpenInNewTabButton
                                        url={appStore.siteSettings ? `${appStore.siteSettings.getblockfrost_url_explorer_address(address)}` : ``}
                                    />
                                </div>
                            </div>

                            {/* Conditional rendering of DetailsBalance or DetailsUTxOs */}
                            {showBalance_ ? (
                                isRefreshing || isLoadingList ? (
                                    <div className={styles.loadingDiv}><LoaderButton /></div>
                                ) : (
                                    <DetailsBalance uTxOs={list} />
                                )
                            ) : (
                                isRefreshing || isLoadingList ? (
                                    <div className={styles.loadingDiv}><LoaderButton /></div>
                                ) : (
                                    <DetailsUTxOs uTxOs={list} />
                                )
                            )}
                        </div>
                    </div>
                </div>
            ) : null}
        </>
    );
    //--------------------------------------
};

