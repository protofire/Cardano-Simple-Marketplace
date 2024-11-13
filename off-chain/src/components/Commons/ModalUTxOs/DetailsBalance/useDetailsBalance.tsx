
import { UTxO } from 'lucid-cardano';
import { useEffect, useState } from 'react';
import {
    getAssetOfUTxOs,
    TokenMetadataFrontEndApiCalls,
    TokensWithMetadataAndAmount,
    useAppStore,
    useDetails
} from 'smart-db';

// Custom hook to manage and load asset details for a given set of UTxOs
export const useDetailsBalance = ({ uTxOs }: { uTxOs: UTxO[] }) => {
    //--------------------------------------
    // State to indicate if the data is currently refreshing
    const [isRefreshing, setIsRefreshing] = useState(true);

    // Effect hook to set `isRefreshing` to false after initial load
    useEffect(() => {
        setIsRefreshing(false);
    }, []);
    //--------------------------------------

    // Access the application store context using custom hook
    const appStore = useAppStore();
    //--------------------------------------

    // Function to load details for all assets in the given UTxOs
    const loadDetails = async () => {
        // Extracts the list of assets from the UTxOs
        const totalAssets = getAssetOfUTxOs(uTxOs);

        // Fetches metadata details for each asset using an API call
        const assetDetails = await TokenMetadataFrontEndApiCalls.getAssetsWithDetailsApi(totalAssets);

        // Returns the detailed information for the assets
        return assetDetails;
    };
    //--------------------------------------

    // useDetails hook provides loading states, current data, and refresh function for asset details
    const { isLoadingDetails, isLoadedDetails, current, refreshDetails } = useDetails<TokensWithMetadataAndAmount>({
        nameDetails: 'Balance', // Identifier name for the detail being loaded
        loadDetails, // Function to load the asset details
    });
    //--------------------------------------

    // Return necessary states, functions, and data for balance details
    return {
        appStore,
        isRefreshing,
        isLoadingDetails,
        isLoadedDetails,
        current,
        refreshDetails,
    };
    //--------------------------------------
};

