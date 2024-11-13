
import Image from 'next/image'; // Importing Next.js Image component for optimized image rendering
import React from 'react'; // Importing React to create the functional component
import { DISCONNECT, IUseWalletStore } from 'smart-db'; // Importing necessary utilities and types from `smart-db`
import WalletApiKey from './WalletApiKey/WalletApiKey'; // Importing the WalletApiKey component to display the API key modal
import styles from './WalletInfo.module.scss'; // Assuming SCSS module for component-specific styles
import { ModalUTxOsAndBalance } from '../../ModalUTxOs/ModalUTxOs'; // Importing the ModalUTxOsAndBalance component for showing UTxOs and balance

// Props interface for the WalletInfo component
interface Props {
  walletStore: IUseWalletStore; // The walletStore holds wallet-related information and functionality
  walletDisconnect: (closeModal?: boolean) => Promise<void>; // Function to disconnect the wallet
}

// Functional component to display wallet information, manage its state and actions
const WalletInfo: React.FC<Props> = ({ walletStore, walletDisconnect }) => {
  //--------------------------------------
  return (
    <>
      {/* Header displaying the wallet type (Seed, Key, or Wallet Name) */}
      <h2 className={styles.walletConnectHeader}>
        YOUR WALLET [
        {
          walletStore.info!.isWalletFromKey === true
            ? 'Key' // Display 'Key' if the wallet was created from a key
            : walletStore.info!.isWalletFromSeed === true
            ? 'Seed' // Display 'Seed' if the wallet was created from a seed
            : walletStore.info!.walletNameOrSeedOrKey // Display wallet name if it's neither a key nor seed
        }]
      </h2>

      {/* Grid container for wallet action buttons and information */}
      <div className={styles.buttonGrid}>
        {/* ModalUTxOsAndBalance component to display UTxOs and balance information */}
        <ModalUTxOsAndBalance 
          address={walletStore.info!.address} 
          uTxOs={walletStore.uTxOsAtWallet} 
          showBalance={true} // Show balance in the modal
        />

        {/* WalletApiKey component to show the API key modal */}
        <WalletApiKey />

        {/* Disconnect wallet button */}
        <button
          key={walletStore.info!.pkh + ' disconnect'} // Use public key hash (pkh) as part of the button's key for uniqueness
          className={styles.walletDisc} // Apply styles for the disconnect button
          onClick={async () => await walletDisconnect(false)} // Trigger wallet disconnect on click
        >
          Disconnect Wallet {/* Text on the button */}
          
          {/* Disconnect icon */}
          <Image
            className={styles.walletImg} // Styles for the image (icon)
            src={DISCONNECT.href} // Path to the disconnect icon (imported from smart-db)
            alt={'Disconnect'} // Alt text for accessibility
            width={20} // Icon width
            height={20} // Icon height
          />
        </button>
      </div>
    </>
  );
};

export default WalletInfo;

