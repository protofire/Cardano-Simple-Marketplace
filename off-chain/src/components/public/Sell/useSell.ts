import { useState } from 'react';
import { getAssetOfUTxOs, TokenMetadataFrontEndApiCalls, TokensWithMetadataAndAmount, useDetails, useWalletStore } from 'smart-db';

export interface SellProps {
    isWalletConnectorModalOpen: boolean;
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>;
}

export const useSell = () => {
    //----------------------------------------------------------------------------
    const walletStore = useWalletStore();
    //----------------------------------------------------------------------------
    // State for managing loading and input values for tokens
    const [isLoading, setIsLoading] = useState(false); // Whether the app is loading data
    //----------------------------------------------------------------------------
    const [isLoadingAnyTx, setIsLoadingAnyTx] = useState<string | undefined>(undefined);
    //----------------------------------------------------------------------------

    // Function to load the details of the user's assets
    const loadDetails = async () => {
        if (walletStore.isWalletDataLoaded === true && walletStore.getUTxOsAtWallet().length>0) {
            const totalAssets = getAssetOfUTxOs(walletStore.getUTxOsAtWallet()); // Fetch wallet's UTxOs
            const assetDetails = await TokenMetadataFrontEndApiCalls.getAssetsWithDetailsApi(totalAssets); // Get metadata details for each asset
            return assetDetails;
        } else {
            return undefined; // Return undefined if wallet data isn't loaded
        }
    };

    // Use the useDetails hook to manage the loading and state of asset details
    const { isLoadingDetails, isLoadedDetails, current } = useDetails<TokensWithMetadataAndAmount>({
        nameDetails: 'Balance',
        loadDetails, // Fetches details using the loadDetails function
        dependencies: [walletStore.isWalletDataLoaded], // Dependency on wallet data being loaded
    });

    return {
        walletStore,
        isLoading,
        isLoadingDetails,
        isLoadedDetails,
        current,
        isLoadingAnyTx,
        setIsLoadingAnyTx,
    };
};
