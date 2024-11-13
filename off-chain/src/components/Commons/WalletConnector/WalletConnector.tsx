import React, { useEffect, useState } from 'react';
import Modal from 'react-modal';
import { useWalletActions } from 'smart-db';
import LoaderButton from '../LoaderButton/LoaderButton';
import styles from './WalletConnector.module.scss'; // Assuming you will create a SCSS module
import WalletInfo from './WalletInfo/WalletInfo';
import WalletList from './WalletList/WalletList';

import { HiUserCircle } from 'react-icons/hi';

const WalletConnector: React.FC = () => {
  //--------------------------------------
  // State to store the private key entered by the user (used for wallet connection)
  const [privateKey, setPrivateKey] = useState<string>();
  //--------------------------------------
  // State to control the visibility of the wallet connection modal
  const [isWalletConnectorModalOpen, setIsWalletConnectorModalOpen] = useState(false);
  //--------------------------------------
  // Destructure necessary functions and state from wallet actions
  const {
    status,
    walletStore,
    createSignedSession,
    walletConnect,
    walletFromSeedConnect,
    walletFromKeyConnect,
    walletInstall,
    walletSelected,
    walletDisconnect,
  } = useWalletActions();
  //--------------------------------------
  // Effect hook to generate a private key when the wallet utility is available
  useEffect(() => {
    const fetch = async () => {
      try {
        if (walletStore._lucidForUseAsUtils === undefined) return;

        // Generate a Bech32 encoded private key using the lucid utility
        const privateKey = walletStore._lucidForUseAsUtils.utils.generatePrivateKey();
        setPrivateKey(privateKey); // Set the generated private key in the state
        console.log(`privateKey: ${privateKey}`); // Log the private key for debugging purposes
      } catch (e) {
        console.error(e); // Catch and log any errors that occur during key generation
      }
    };
    fetch(); // Call the fetch function when the component mounts or when the wallet store changes
  }, [walletStore._lucidForUseAsUtils]); // Dependency on walletStore._lucidForUseAsUtils to re-trigger effect

  //--------------------------------------
  // Handler for opening the wallet connection modal
  const handleBtnConnectWallet = async () => {
    setIsWalletConnectorModalOpen(true); // Open the modal when the button is clicked
  };
  //--------------------------------------

  return (
    <>
      {/* Button to trigger the wallet connection modal */}
      <button
        className={styles.buttonCenterWithLoading}
        onClick={() => {
          if (walletStore.isGettingWalletsDone === true) {
            handleBtnConnectWallet(); // Open the modal only if wallets are loaded
          }
        }}
      >
        {/* Display wallet icon and truncated wallet address */}
        <HiUserCircle className={styles.icon} />
        <p className={styles.walletAddress}>
          {/* If wallet address is available, display last part of the address */}
          {walletStore.info?.address ? `...${walletStore.info?.address.substring(102)}` : ''}
        </p>
        {/* Show loading button if status is loading or any wallet connection process is ongoing */}
        {(status === 'loading' || walletStore.isGettingWalletsDone === false || walletStore.isConnecting || walletStore.isLoadingAnyData) && <LoaderButton />}
      </button>

      {/* Modal to manage wallet connection */}
      <Modal
        isOpen={isWalletConnectorModalOpen} // Modal visibility controlled by state
        onRequestClose={() => setIsWalletConnectorModalOpen(false)} // Close modal when requested
        contentLabel="Connect Wallet"
        className={styles.modal}
        overlayClassName={styles.overlay}
      >
        <div className={styles.modalContent}>
          {/* If wallet is not connected, show connection options */}
          {walletStore.isConnected === false ? (
            <>
              <h2 className={styles.walletConnectHeader}>Connect Wallet</h2>
              <text className={styles.walletConnectText}>Wallet Private Key:</text>
              <div className={styles.grid}>
                {/* Input field for entering private key */}
                <input 
                  name="privateKey" 
                  value={privateKey ?? ''} 
                  onChange={(e) => setPrivateKey(e.target.value)} 
                  className={styles.inputField} 
                />
                {/* Button to connect using the private key */}
                <button
                  className={styles.buttonConnectWithKey}
                  onClick={async () => {
                    if (privateKey !== undefined) {
                      // Connect wallet using the provided private key
                      await walletFromKeyConnect(privateKey, createSignedSession, true, false);
                    }
                  }}
                >
                  Connect With Key
                </button>
              </div>
              {/* Display a list of available wallets for the user to select */}
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
              {/* If wallet is connected, display wallet info and option to disconnect */}
              <WalletInfo walletStore={walletStore} walletDisconnect={walletDisconnect} />
            </>
          )}
          {/* Close button to close the modal */}
          <button className={styles.buttonClose} onClick={() => setIsWalletConnectorModalOpen(false)}>
            Close
          </button>
        </div>
      </Modal>
    </>
  );
};

// Set the app element for accessibility reasons (required for Modal in React)
Modal.setAppElement('#__next');

export default WalletConnector;

