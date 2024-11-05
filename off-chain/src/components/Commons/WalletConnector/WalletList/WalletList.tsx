import Image from 'next/image';
import React from 'react';
import { CardanoWallet, IUseWalletStore } from 'smart-db';
import LoaderButton from '../../LoaderButton/LoaderButton';
import styles from './WalletList.module.scss'; // Assuming you will create a SCSS module

interface Props {
  walletStore: IUseWalletStore;
  walletSelected: string | undefined;
  walletConnect: (wallet: CardanoWallet, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean, tryAgain?: boolean) => Promise<void>;
  walletFromSeedConnect: (walletSeed: string, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean) => Promise<void>;
  walletFromKeyConnect: (walletKey: string, createSignedSession: boolean, forceConnect?: boolean, closeModal?: boolean) => Promise<void>;
  walletInstall: (wallet: CardanoWallet) => Promise<void>;
  createSignedSession: boolean;
}

const WalletList: React.FC<Props> = ({ walletStore, walletSelected, walletConnect, walletFromSeedConnect, walletFromKeyConnect, walletInstall, createSignedSession }) => {
  //--------------------------------------
  return (
    <>
        <div className="grid grid-cols-2 gap-4 w-full">
          {walletStore.cardanoWallets.map((wallet, index) => (
            <div key={index}>
              <button
                key={wallet.wallet}
                className={`flex items-center justify-start p-3 rounded ${wallet.isInstalled ? 'bg-zinc-700 hover:bg-zinc-600' : 'bg-zinc-900 hover:bg-zinc-800 opacity-50'
                  } text-white`}
                disabled={!wallet.isInstalled}
                onClick={async () => {
                  if (wallet.isInstalled) {
                    await walletConnect(wallet, createSignedSession, true, false, true);
                  }
                }}
              >
                <div className={`flex items-center justify-start p-3 rounded text-white`}>
                  <Image className="mr-2" src={wallet.icon.href} alt={wallet.name} width={30} height={30} />
                  <p className="ml-auto text-xs text-zinc-200 hover:text-white ">{wallet.name}</p>
                  {walletSelected === wallet.wallet && <LoaderButton />}
                </div>
                {!wallet.isInstalled && (
                  <div className="ml-auto text-xs text-zinc-200 hover:text-white " onClick={async () => await walletInstall(wallet)}>
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
