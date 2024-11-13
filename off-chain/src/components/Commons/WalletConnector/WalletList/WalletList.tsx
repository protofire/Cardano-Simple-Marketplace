import Image from 'next/image';
import React from 'react';
import { CardanoWallet, IUseWalletStore } from 'smart-db';
import LoaderButton from '../../LoaderButton/LoaderButton';
import styles from './WalletList.module.scss'; // Assuming you will create a SCSS module

// Define props for WalletList component
interface Props {
  walletStore: IUseWalletStore; // The wallet store that contains information about available wallets
  walletSelected: string | undefined; // The wallet currently selected
  walletConnect: (wallet: CardanoWallet, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean, tryAgain?: boolean) => Promise<void>; // Function to connect the wallet
  walletFromSeedConnect: (walletSeed: string, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean) => Promise<void>; // Function to connect wallet using seed phrase
  walletFromKeyConnect: (walletKey: string, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean) => Promise<void>; // Function to connect wallet using private key
  walletInstall: (wallet: CardanoWallet) => Promise<void>; // Function to install the wallet if it's not installed
  createSignedSession: boolean; // Flag to determine if a signed session is to be created
}

const WalletList: React.FC<Props> = ({ walletStore, walletSelected, walletConnect, walletInstall, createSignedSession }) => {
  //--------------------------------------
  return (
    <>
      {/* Displaying a grid of wallets */}
      <div className={styles.walletGrid}>
        {/* Iterate over all available Cardano wallets */}
        {walletStore.cardanoWallets.map((wallet, index) => (
          <div key={index}>
            {/* Button for each wallet */}
            <button
              key={wallet.wallet}
              // Apply different styles based on whether the wallet is installed or not
              className={`${styles.walletButton} ${wallet.isInstalled ? styles.installed : styles.notInstalled}`}
              disabled={!wallet.isInstalled} // Disable button if wallet is not installed
              onClick={async () => {
                if (wallet.isInstalled) {
                  // If the wallet is installed, connect it
                  await walletConnect(wallet, createSignedSession, true, false, true);
                }
              }}
            >
              {/* Inner content of the wallet button */}
              <div className={styles.walletButtonInner}>
                {/* Display wallet icon */}
                <Image className={styles.walletIcon} src={wallet.icon.href} alt={wallet.name} width={30} height={30} />
                {/* Display wallet name */}
                <p className={styles.walletName}>{wallet.name}</p>
                {/* Show loading button if the wallet is selected */}
                {walletSelected === wallet.wallet && <LoaderButton />}
              </div>

              {/* If the wallet is not installed, show the "Not Installed" text and allow user to install */}
              {!wallet.isInstalled && (
                <div className={styles.notInstalledText} onClick={async () => await walletInstall(wallet)}>
                  Not Installed
                </div>
              )}
            </button>
          </div>
        ))}
      </div>
    </>
  );
};

export default WalletList;

