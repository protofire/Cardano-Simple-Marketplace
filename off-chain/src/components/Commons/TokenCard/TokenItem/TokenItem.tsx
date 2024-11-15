
// TokenItem.tsx
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities'; // Import MarketNFTEntity type for token entity data
import React from 'react'; // React library
import { formatTokenNameHexToStr, getUrlForImage, isValidUrl, TOKEN_ICON_GENERIC, Token_With_Metadata_And_Amount, TokenMetadataEntity } from 'smart-db'; // Import helper functions and constants for handling tokens and URLs
import styles from './TokenItem.module.scss'; // Import styles for TokenItem component

// Type for MarketNFT entity with its associated metadata
type MarketNFTEntityWithMetadata = {
  entity: MarketNFTEntity; // The token entity data
  metadata: TokenMetadataEntity; // The metadata associated with the token
};

// Props interface for TokenItem component
interface TokenItemProps {
  tokenToBuy?: MarketNFTEntityWithMetadata; // Token to buy (optional)
  tokenToSell?: Token_With_Metadata_And_Amount; // Token to sell (optional)
  sell: boolean; // Boolean indicating if the item is for sale or purchase
}

// TokenItem component: Displays token information based on whether it's for sale or purchase
const TokenItem: React.FC<TokenItemProps> = ({ tokenToBuy, tokenToSell, sell }) => {
  // Conditional rendering based on whether the token is for sale or purchase
  return sell ? (
    // If selling, display the token's image, name, and amount
    <section>
      <div className={styles.tokenImage}>
        {/* Check if the image URL is valid, display the image or default icon */}
        {isValidUrl(tokenToSell!.image!) ? (
          <img src={getUrlForImage(tokenToSell!.image!!)} alt={`logo-${formatTokenNameHexToStr(tokenToSell!.TN_Hex)}`} className={styles.tokenImage} />
        ) : (
          <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(tokenToSell!.TN_Hex)} className={styles.assetImage} />
        )}
      </div>
      <div className={styles.tokenNameInscription}>
        <p className={styles.tokenName}>{formatTokenNameHexToStr(tokenToSell!.TN_Hex)}</p> {/* Display token name */}
        <p className={styles.tokenAmount}>[{tokenToSell!.amount.toString()}]</p> {/* Display token amount */}
      </div>
    </section>
  ) : (
    // If buying, display the token's image and name
    <section>
      <div className={styles.tokenImage}>
        {/* Check if the image URL is valid, display the image or default icon */}
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
      <p className={styles.tokenName}>{formatTokenNameHexToStr(tokenToBuy!.entity.sellingToken_TN)}</p> {/* Display token name */}
    </section>
  );
};

export default TokenItem; // Export TokenItem component for use in other parts of the application

