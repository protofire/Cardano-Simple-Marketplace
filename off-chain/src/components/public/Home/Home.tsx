// Home.tsx
import { useHome } from './useHome';
import { Sell } from '@/components/public/Sell/Sell';
import { Buy } from '../Buy/Buy';
import WalletConnector from '../../Commons/WalletConnector/WalletConnector';
import styles from './Home.module.scss';

export default function Home() {
    const { isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, menuClass, handleClickMenuClass } = useHome();

    return (
        <main className={styles.main}>
            {/* USER LOGGED IN */}
            <header className={styles.navbar}>
                {/* Buttons for Buy and Sell, aligned to the left */}
                <div className={styles.buttonContainer}>
                    <button onClick={() => handleClickMenuClass('Buy')} className={`${menuClass === 'Buy' ? styles.buy : styles.inactive} ${styles.button}`}>
                        Buy
                    </button>
                    <button onClick={() => handleClickMenuClass('Sell')} className={`${menuClass === 'Sell' ? styles.buy : styles.inactive} ${styles.button}`}>
                        Sell
                    </button>
                </div>

                {/* Wallet selector, aligned to the right */}
                <div className={styles.walletConnector}>
                    {<WalletConnector isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}
                </div>
            </header>

            {/* Content section that conditionally renders the Buy or Sell component */}
            <div className={styles.content}>
                {menuClass === 'Buy' && <Buy isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}
                {menuClass === 'Sell' && <Sell isWalletConnectorModalOpen={isWalletConnectorModalOpen} setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} />}
            </div>
        </main>
    );
}

