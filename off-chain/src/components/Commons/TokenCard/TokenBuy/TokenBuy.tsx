// TokenBuy.tsx
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import { lovelaceToAda } from '@example/src/lib/Commons/Constants/onchain';
import React from 'react';
import ModalTransaction from '../../ModalTransaction/ModalTransaction';
import styles from './TokenBuy.module.scss';
import { TokenBuyProps, useTokenBuy } from './useTokenBuy';

const TokenBuy: React.FC<TokenBuyProps> = ({ tokenToBuy, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    const { walletStore, isLoadingTxWithdraw, isLoadingTxBuy, isTxModalOpen, txHash, isTxError, txMessage, txConfirmed, handleBtnWithdrawTx, handleBtnBuyTx, setIsTxModalOpen } =
        useTokenBuy({
            tokenToBuy,
            isWalletConnectorModalOpen,
            setIsWalletConnectorModalOpen,
            isLoadingAnyTx,
            setIsLoadingAnyTx,
        });

    return (
        <section>
            <div className={styles.price}>{(Number(tokenToBuy.entity.priceOfAsset) / Number(lovelaceToAda)).toFixed(6)} ₳</div>
            {!walletStore.isConnected ? (
                <button className={styles.invalidButton} onClick={() => setIsWalletConnectorModalOpen(true)}>
                    Connect
                </button>
            ) : walletStore.info?.pkh === tokenToBuy.entity.sellerPaymentPKH ? (
                <button className={styles.withdrawButton} onClick={() => handleBtnWithdrawTx()}>
                    Withdraw
                    {isLoadingTxWithdraw && <LoaderButton />} {/* Show loader when withdrawing */}
                </button>
            ) : (
                <button className={styles.buyButton} onClick={() => handleBtnBuyTx()}>
                    Buy
                    {isLoadingTxBuy && <LoaderButton />} {/* Show loader when buying */}
                </button>
            )}
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

export default TokenBuy;
