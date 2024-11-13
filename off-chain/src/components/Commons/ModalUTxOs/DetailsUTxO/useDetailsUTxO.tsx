import { UTxO } from 'lucid-cardano';
import { useEffect, useState } from 'react';
import { TokenMetadataFrontEndApiCalls, useAppStore, useList, UTxOWithDetails } from 'smart-db';

export const useDetailsUTxOs = ({ uTxOs }: { uTxOs: UTxO[] }) => {
    //--------------------------------------
    const [isRefreshing, setIsRefreshing] = useState(true);

    // Reset `isRefreshing` to `false` after the first render to track refresh status
    useEffect(() => {
        setIsRefreshing(false);
    }, []);
    //--------------------------------------

    // Retrieve global app store for access to application settings
    const appStore = useAppStore();
    //--------------------------------------

    // Function to load the detailed list of UTxOs with asset metadata
    const loadList = async () => {
        const uTxOsWithDetails = await Promise.all(
            uTxOs.map(async (key) => {
                // Retrieve detailed information for each asset in the UTxO
                const assetWithDetails = await TokenMetadataFrontEndApiCalls.getAssetsWithDetailsApi(key.assets);

                // Return an object with UTxO properties and extra information about assets and script reference
                return {
                    ...key,
                    assetWithDetails,
                    hasScriptRef: key.scriptRef !== undefined ? 'Yes' : 'No',
                };
            })
        );
        return uTxOsWithDetails;
    };
    //--------------------------------------

    // Use `useList` hook to handle loading, storing, and refreshing the list of UTxOs with details
    const { isLoadingList, isLoadedList, list, refreshList } = useList<UTxOWithDetails>({
        nameList: 'UTxOWithDetails',
        loadList,
    });
    //--------------------------------------

    // Return necessary states and data for the component to access
    return {
        appStore,
        isRefreshing,
        isLoadingList,
        isLoadedList,
        uTxOsWithDetails: list, // The list of UTxOs with asset details
        refreshList,
    };
    //--------------------------------------
};
