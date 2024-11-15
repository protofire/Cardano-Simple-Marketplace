// TokenSell.tsx
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import React from 'react';
import ModalTransaction from '../../ModalTransaction/ModalTransaction';
import styles from './TokenSell.module.scss';
import { TokenSellProps, useTokenSell } from './useTokenSell';

// TODO: ya no hace falta [inputValues, setInputValues] = useState<{ [key: string]: string }>({});
// Por que ahora al estar separado en un nuevo componente cada uno tiene su estado, y ya no se pisan entre si como antes
// entonces no hace falta el array o list, con un solo valor para input y uno solo para el boolean si hay tx esta bien

// Se complica que ahora es mas complejo manejar el estado general si hay any tx, para no permitir mas de una tx al mismo tiempo
// para eso la pagina principal sell tiene ese estado y enviar el setter y el estado a los subcomponentes, cosa de que desde alli se pueda setear y revisar ese estado

const TokenSell: React.FC<TokenSellProps> = ({ tokenToSell, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    const {
        walletStore,
        inputValue,
        isLoadingTxSell,
        isTxModalOpen,
        txHash,
        isTxError,
        txMessage,
        txConfirmed,
        handleBtnSellTx,
        handleInputChange,
        setIsTxModalOpen,
        isValidInput,
    } = useTokenSell({
        tokenToSell,
        isWalletConnectorModalOpen,
        setIsWalletConnectorModalOpen,
        isLoadingAnyTx,
        setIsLoadingAnyTx,
    });

    return (
        <section>
            <div className={styles.inputContainer}>
                <input value={inputValue} onChange={handleInputChange} type="text" placeholder="Lovelace" className={styles.input} />
                {!walletStore.isConnected ? (
                    <button className={styles.invalidButton} onClick={() => setIsWalletConnectorModalOpen(true)}>
                        Connect
                    </button>
                ) : (
                    <button
                        className={
                            !isValidInput || !(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToSell.CS + tokenToSell.TN_Hex) ? styles.invalidButton : styles.sellButton
                        }
                        disabled={!isValidInput || !(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToSell.CS + tokenToSell.TN_Hex)}
                        onClick={handleBtnSellTx}
                    >
                        Sell {isLoadingTxSell && <LoaderButton />}
                    </button>
                )}
            </div>
            <ModalTransaction
                isOpen={isTxModalOpen}
                onRequestClose={() => setIsTxModalOpen(false)}
                txMessage={txMessage}
                txHash={txHash!}
                txConfirmed={txConfirmed}
                isTxError={isTxError}
            />
        </section>
    );
};

export default TokenSell;
