import React from 'react';
import Modal from 'react-modal'; // Importing the Modal component for displaying the wallet API key in a popup
import LoaderButton from '../../../LoaderButton/LoaderButton'; // Importing a button component that shows a loading spinner while data is loading
import { useWalletApiKey } from './useWalletApiKey'; // Importing the custom hook to manage the API key logic and state
import styles from './WalletApiKey.module.scss'; // Importing the styles for this component (assuming SCSS module)

interface Props {}

// Functional component to display the Wallet API Key modal
const WalletApiKey: React.FC<Props> = ({ }) => {
  //--------------------------------------
  // Destructure the necessary state and functions from the custom hook `useWalletApiKey`
  const { 
    walletStore, 
    isLoadedDetails, 
    isLoadingDetails, 
    isReady, 
    apiToken, 
    isOpen, 
    handleOpen, 
    handleClose, 
    handleCopy 
  } = useWalletApiKey();
  //--------------------------------------

  return (
    <>
      {/* Display the "Api Key" button only if the wallet is ready */}
      {isReady ? (
        <button
          onClick={() => {
            // Open the modal if the wallet is done loading
            if (walletStore.isGettingWalletsDone === true) handleOpen();
          }}
          className={styles.apiKey}
        >
          Api Key
        </button>
      ) : null}

      {/* Modal component to display the Wallet API Key */}
      <Modal
        isOpen={isOpen} // Modal will be open if `isOpen` state is true
        onRequestClose={() => handleClose()} // Function to close the modal when the request to close is triggered
        contentLabel="Wallet Api Key" // Accessibility label for the modal content
        className={styles.modal} // Custom styles for the modal content
        overlayClassName={styles.overlay} // Custom styles for the modal overlay (background behind the modal)
      >
        <div className={styles.walletApiKeyContainer}>
          <h2>Wallet Api Key</h2>

          {/* Show a loading spinner if the details are not loaded or are loading */}
          {!isLoadedDetails || isLoadingDetails ? (
            <div className={styles.loadingDiv}>
              <LoaderButton /> {/* Button with loading spinner */}
            </div>
          ) : (
            // Show the API token in a read-only textarea if details are loaded
            <textarea
              rows={12} 
              cols={60} 
              value={apiToken} 
              readOnly={true} 
              style={{ fontSize: '8px', background: '#2d3748' }} 
            />
          )}

          {/* Button to copy the API token to the clipboard */}
          <button onClick={() => handleCopy()} className={styles.buttonCopy}>
            Copy
          </button>

          {/* Button to close the modal */}
          <button onClick={() => handleClose()} className={styles.buttonClose}>
            Close
          </button>
        </div>
      </Modal>
    </>
  );
};

// Accessibility improvement to bind the modal to the main app element
Modal.setAppElement('#__next');

export default WalletApiKey;

