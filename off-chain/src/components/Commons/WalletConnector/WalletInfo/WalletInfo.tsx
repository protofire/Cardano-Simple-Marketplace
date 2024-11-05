import Image from 'next/image';
import React from 'react';
import { DISCONNECT, IUseWalletStore } from 'smart-db';
import WalletApiKey from './WalletApiKey/WalletApiKey';
import styles from './WalletInfo.module.scss'; // Assuming you will create a SCSS module
import { ModalUTxOsAndBalance } from '../../ModalUTxOs/ModalUTxOs';

interface Props {
    walletStore: IUseWalletStore;
    walletDisconnect: (closeModal?: boolean) => Promise<void>;
}

const WalletInfo: React.FC<Props> = ({ walletStore, walletDisconnect }) => {
    //--------------------------------------
    return (
        <>
            <h2 className="text-xl font-bold mb-4 text-white">
                YOUR WALLET [{walletStore.info!.isWalletFromKey === true ? 'Key' : walletStore.info!.isWalletFromSeed === true ? 'Seed' : walletStore.info!.walletNameOrSeedOrKey}]
            </h2>
            <div className="grid gap-y-4 items-center">
                <ModalUTxOsAndBalance address={walletStore.info!.address} uTxOs={walletStore.uTxOsAtWallet}  showBalance={true}/>
                <WalletApiKey />
                <button key={walletStore.info!.pkh + ' disconnect'} className={styles.walletDetails} onClick={async () => await walletDisconnect(false)}>
                    <p className="text-l font-bold mb-4 text-white">Disconnect Wallet <Image className={styles.walletImg} src={DISCONNECT.href} alt={'Disconnect'} width={20} height={20}></Image></p>
                </button>
            </div>
        </>
    );
};

export default WalletInfo;
