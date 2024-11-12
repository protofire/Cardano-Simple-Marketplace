import { Lucid } from 'lucid-cardano';
import React, { useEffect, useState } from 'react';
import Modal from 'react-modal';
import { useWalletActions } from 'smart-db';
import LoaderButton from '../LoaderButton/LoaderButton';
import styles from './WalletConnector.module.scss'; // Assuming you will create a SCSS module
import WalletInfo from './WalletInfo/WalletInfo';
import WalletList from './WalletList/WalletList';

import { HiUserCircle } from 'react-icons/hi';
import { IoReloadCircleSharp } from 'react-icons/io5';

const WalletConnector: React.FC = () => {
  //--------------------------------------
  const [privateKey, setPrivateKey] = useState<string>();
  //--------------------------------------
  const [isWalletConnectorModalOpen, setIsWalletConnectorModalOpen] = useState(false);
  //--------------------------------------
  const {
    isRefreshing: isRefreshingWallet,
    session,
    status,
    walletStore,
    createSignedSession,
    setCreateSignedSession,
    walletConnect,
    walletFromSeedConnect,
    walletFromKeyConnect,
    walletInstall,
    walletSelected,
    setWalletSelected,
    walletDisconnect,
  } = useWalletActions();
  //--------------------------------------
  useEffect(() => {
    const fetch = async () => {
      try {
        if (walletStore._lucidForUseAsUtils === undefined) return;

        const privateKey = walletStore._lucidForUseAsUtils.utils.generatePrivateKey(); // Bech32 encoded private key
        setPrivateKey(privateKey);
        console.log(`privateKey: ${privateKey}`);
      } catch (e) {
        console.error(e);
      }
    };
    fetch();
  }, [walletStore._lucidForUseAsUtils]);
  //--------------------------------------
  const handleBtnConnectWallet = async () => {
    setIsWalletConnectorModalOpen(true);
  };
  //--------------------------------------

  return (
    <>
      <button
        className={styles.buttonCenterWithLoading}
        onClick={() => {
          if (walletStore.isGettingWalletsDone === true) {
            handleBtnConnectWallet();
          }
        }}
      >
        <HiUserCircle className={styles.icon} />
        <p className={styles.walletAddress}>{walletStore.info?.address ? `...${walletStore.info?.address.substring(102)}` : ''}</p>
        {(status === 'loading' || walletStore.isGettingWalletsDone === false || walletStore.isConnecting || walletStore.isLoadingAnyData) && <LoaderButton />}
      </button>

      <Modal
        isOpen={isWalletConnectorModalOpen}
        onRequestClose={() => setIsWalletConnectorModalOpen(false)}
        contentLabel="Connect Wallet"
        className={styles.modal}
        overlayClassName={styles.overlay}
      >
        <div className={styles.modalContent}>
          {walletStore.isConnected === false ? (
            <>
              <h2 className={styles.walletConnectHeader}>Connect Wallet</h2>
              <text className={styles.walletConnectText}>Wallet Private Key:</text>
              <div className="grid gap-y-4 items-center">
                <input name="privateKey" value={privateKey ?? ''} onChange={(e) => setPrivateKey(e.target.value)} className={styles.inputField} />
                <button
                  className={styles.buttonConnectWithKey}
                  onClick={async () => {
                    if (privateKey !== undefined) {
                      await walletFromKeyConnect(privateKey, createSignedSession, true, false);
                    }
                  }}
                >
                  Connect With Key
                </button>
              </div>
              <WalletList
                walletStore={walletStore}
                walletSelected={walletSelected}
                walletConnect={walletConnect}
                walletInstall={walletInstall}
                walletFromSeedConnect={walletFromSeedConnect}
                walletFromKeyConnect={walletFromKeyConnect}
                createSignedSession={createSignedSession}
              />
            </>
          ) : (
            <>
              <WalletInfo walletStore={walletStore} walletDisconnect={walletDisconnect} />
            </>
          )}
          <button className={styles.buttonClose} onClick={() => setIsWalletConnectorModalOpen(false)}>
            Close
          </button>
        </div>
      </Modal>
    </>
  );
};
Modal.setAppElement('#__next');

export default WalletConnector;
