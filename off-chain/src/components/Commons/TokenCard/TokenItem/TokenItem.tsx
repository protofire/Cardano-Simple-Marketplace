// TokenItem.tsx
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import React from 'react';
import { formatTokenNameHexToStr, getUrlForImage, isValidUrl, TOKEN_ICON_GENERIC, Token_With_Metadata_And_Amount, TokenMetadataEntity } from 'smart-db';
import styles from './TokenItem.module.scss';

type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

interface TokenItemProps {
    tokenToBuy?: MarketNFTEntityWithMetadata;
    tokenToSell?: Token_With_Metadata_And_Amount;
    sell: boolean;
}

const TokenItem: React.FC<TokenItemProps> = ({ tokenToBuy, tokenToSell, sell }) => {
    return sell ? (
        <section>
            <div className={styles.tokenImage}>
                {isValidUrl(tokenToSell!.image!) ? (
                    <img src={getUrlForImage(tokenToSell!.image!!)} alt={`logo-${formatTokenNameHexToStr(tokenToSell!.TN_Hex)}`} className={styles.tokenImage} />
                ) : (
                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(tokenToSell!.TN_Hex)} className={styles.assetImage} />
                )}
            </div>
            <p className={styles.tokenName}>{formatTokenNameHexToStr(tokenToSell!.TN_Hex)}</p>
            <p className={styles.tokenAmount}>[{tokenToSell!.amount.toString()}]</p>
        </section>
    ) : (
        <section>
            <div className={styles.tokenImage}>
                {isValidUrl(tokenToBuy!.metadata.image) ? (
                    <img
                        src={getUrlForImage(tokenToBuy!.metadata.image!)}
                        alt={`logo-${formatTokenNameHexToStr(tokenToBuy!.entity.sellingToken_TN)}`}
                        className={styles.tokenImage}
                    />
                ) : (
                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(tokenToBuy!.entity.sellingToken_TN)} className={styles.assetImage} />
                )}
            </div>
            <p className={styles.tokenName}>{formatTokenNameHexToStr(tokenToBuy!.entity.sellingToken_TN)}</p>
        </section>
    );
};

export default TokenItem;
