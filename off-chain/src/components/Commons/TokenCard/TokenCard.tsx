// TokenCard.tsx
import TokenItem from '@/components/Commons/TokenCard/TokenItem/TokenItem';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import React from 'react';
import { Token_With_Metadata_And_Amount, TokenMetadataEntity } from 'smart-db';
import TokenBuy from './TokenBuy/TokenBuy';
import styles from './TokenCard.module.scss';
import TokenSell from './TokenSell/TokenSell';

// TODO: agregar commentarios

type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

interface TokenCardProps {
    tokenToBuy?: MarketNFTEntityWithMetadata;
    tokenToSell?: Token_With_Metadata_And_Amount;
    sell: boolean;
    isWalletConnectorModalOpen: boolean;
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>;
    isLoadingAnyTx: string | undefined; // Pass down from parent
    setIsLoadingAnyTx: React.Dispatch<React.SetStateAction<string | undefined>>; // Pass down from parent
}

const TokenCard: React.FC<TokenCardProps> = ({ tokenToBuy, tokenToSell, sell, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    const token = sell ? tokenToSell : tokenToBuy?.metadata;

    return (
        <div className={styles.tokenCard}>
            <TokenItem tokenToBuy={tokenToBuy} tokenToSell={tokenToSell} sell={sell} />
            {!sell ? (
                <TokenBuy
                    tokenToBuy={tokenToBuy!}
                    isWalletConnectorModalOpen={isWalletConnectorModalOpen}
                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen}
                    isLoadingAnyTx={isLoadingAnyTx}
                    setIsLoadingAnyTx={setIsLoadingAnyTx}
                />
            ) : (
                <TokenSell
                    tokenToSell={tokenToSell!}
                    isWalletConnectorModalOpen={isWalletConnectorModalOpen}
                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen}
                    isLoadingAnyTx={isLoadingAnyTx}
                    setIsLoadingAnyTx={setIsLoadingAnyTx}
                />
            )}
        </div>
    );
};

export default TokenCard;
