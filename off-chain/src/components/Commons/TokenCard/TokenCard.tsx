
// TokenCard.tsx
import TokenItem from '@/components/Commons/TokenCard/TokenItem/TokenItem'; // Import TokenItem component to display basic token details
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities'; // Import the MarketNFTEntity model
import React from 'react'; // React library
import { Token_With_Metadata_And_Amount, TokenMetadataEntity } from 'smart-db'; // Import additional types for token metadata
import TokenBuy from './TokenBuy/TokenBuy'; // Import TokenBuy component for handling token purchase
import styles from './TokenCard.module.scss'; // Import styles for the TokenCard component
import TokenSell from './TokenSell/TokenSell'; // Import TokenSell component for handling token sale

// Define a type to combine a MarketNFTEntity with its associated metadata
type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

// Define the TokenCardProps interface for component props, including optional token data for buying or selling, and state management for wallet and transaction loading
interface TokenCardProps {
    tokenToBuy?: MarketNFTEntityWithMetadata; // Token data for buying (optional)
    tokenToSell?: Token_With_Metadata_And_Amount; // Token data for selling (optional)
    sell: boolean; // Boolean indicating whether this is a buy or sell action
    isWalletConnectorModalOpen: boolean; // State indicating if the wallet connector modal is open
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>; // Function to set the state of the wallet modal
    isLoadingAnyTx: string | undefined; // Transaction loading state passed down from parent
    setIsLoadingAnyTx: React.Dispatch<React.SetStateAction<string | undefined>>; // Function to set transaction loading state passed down from parent
}

// Main TokenCard component
const TokenCard: React.FC<TokenCardProps> = ({ tokenToBuy, tokenToSell, sell, isWalletConnectorModalOpen, setIsWalletConnectorModalOpen, isLoadingAnyTx, setIsLoadingAnyTx }) => {
    return (
        <div className={styles.tokenCard}> {/* Wrapper for the token card */}
            {/* Display basic token information using the TokenItem component */}
            <TokenItem tokenToBuy={tokenToBuy} tokenToSell={tokenToSell} sell={sell} />

            {/* Conditionally render the TokenBuy or TokenSell component based on the 'sell' prop */}
            {!sell ? (
                // TokenBuy component is rendered when 'sell' is false (for buying tokens)
                <TokenBuy
                    tokenToBuy={tokenToBuy!} // Pass tokenToBuy data
                    isWalletConnectorModalOpen={isWalletConnectorModalOpen} // Pass wallet modal state
                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} // Function to update wallet modal state
                    isLoadingAnyTx={isLoadingAnyTx} // Pass loading state for transactions
                    setIsLoadingAnyTx={setIsLoadingAnyTx} // Function to update loading state for transactions
                />
            ) : (
                // TokenSell component is rendered when 'sell' is true (for selling tokens)
                <TokenSell
                    tokenToSell={tokenToSell!} // Pass tokenToSell data
                    isWalletConnectorModalOpen={isWalletConnectorModalOpen} // Pass wallet modal state
                    setIsWalletConnectorModalOpen={setIsWalletConnectorModalOpen} // Function to update wallet modal state
                    isLoadingAnyTx={isLoadingAnyTx} // Pass loading state for transactions
                    setIsLoadingAnyTx={setIsLoadingAnyTx} // Function to update loading state for transactions
                />
            )}
        </div>
    );
};

export default TokenCard; // Export the TokenCard component for use in other parts of the application

