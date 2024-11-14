// TokenCard.tsx
import React from 'react';
import TokenItem from '@/components/Commons/TokenCard/TokenItem/TokenItem';
import styles from './TokenCard.module.scss';

import {
    Token_With_Metadata_And_Amount,
    TokenMetadataEntity,
} from 'smart-db';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import TokenBuy from './TokenBuy/TokenBuy';
import TokenSell from './TokenSell/TokenSell';

type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

interface TokenCardProps {
    tokenToBuy?: MarketNFTEntityWithMetadata;
    tokenToSell?: Token_With_Metadata_And_Amount;
    walletConnected: boolean;
    sell: boolean;
    isOwnerToken?: boolean;
}

const TokenCard: React.FC<TokenCardProps> = ({ tokenToBuy, tokenToSell, walletConnected, sell, isOwnerToken }) => {
    const token = sell ? tokenToSell : tokenToBuy?.metadata;

    return (
        <div className={styles.tokenCard}>
            <TokenItem token={token!} />
            {!sell ? (
                <TokenBuy tokenToBuy={tokenToBuy!} walletConnected={walletConnected} isOwnerToken={isOwnerToken!} />
            ) : (
                <TokenSell tokenToSell={tokenToSell!} />
            )}
        </div>
    );
};

export default TokenCard;
