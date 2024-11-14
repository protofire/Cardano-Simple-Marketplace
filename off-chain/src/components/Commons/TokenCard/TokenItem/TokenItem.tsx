// TokenItem.tsx
import React from 'react';
import styles from './TokenItem.module.scss';

import {
    formatTokenNameHexToStr,
    getUrlForImage,
    isValidUrl,
    TOKEN_ICON_GENERIC,
    Token_With_Metadata,
    TokenMetadataEntity,
} from 'smart-db';

interface TokenItemProps {
    token: Token_With_Metadata | TokenMetadataEntity;
}

const TokenItem: React.FC<TokenItemProps> = ({ token }) => {
    return (
        <section>
            <div className={styles.tokenImage}>
                {isValidUrl(token.image) ? (
                    <img src={getUrlForImage(token.image!)} alt={`logo-${formatTokenNameHexToStr(token.TN_Hex)}`} className={styles.tokenImage} />
                ) : (
                    <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(token.TN_Hex)} className={styles.assetImage} />
                )}
            </div>
            <p className={styles.tokenName}>{formatTokenNameHexToStr(token.TN_Hex)}</p>
        </section>
    );
};

export default TokenItem;
