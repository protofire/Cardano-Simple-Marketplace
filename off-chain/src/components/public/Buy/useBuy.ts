import { AppStateContext } from '@/pages/_app';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { useContext, useState } from 'react';
import { pushSucessNotification, pushWarningNotification, TokenMetadataEntity, TokenMetadataFrontEndApiCalls, useList } from 'smart-db';

export interface BuyProps {
    // This interface defines the props required for managing the wallet connector modal state.
    isWalletConnectorModalOpen: boolean; // Flag indicating if the wallet connector modal is open
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>; // Function to update the modal state
}

// This type is used to manage a list of MarketNFTs with their associated metadata. 
// First, the list of MarketNFT entities is fetched, and then the metadata for each token is requested and added to the list.
type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity; // The MarketNFT entity itself
    metadata: TokenMetadataEntity; // The metadata for the token associated with the entity
};

export const useBuy = () => {
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext); // Access the app state using React context
    const { marketAddress } = appState; // Extract the market address from the app state
    //----------------------------------------------------------------------------
    // State variables to manage loading states and the transaction modal
    const [isLoading, setIsLoading] = useState(false); // Tracks if the component is loading
    const [isLoadingSync, setIsLoadingSync] = useState(false); // Tracks synchronization loading state
    const [isLoadingAnyTx, setIsLoadingAnyTx] = useState<string | undefined>(undefined); // Tracks the loading state for any transaction
    //----------------------------------------------------------------------------
    //
    // Function to load a list of MarketNFTs with metadata
    const loadList = async () => {
        // Fetch all MarketNFT entities with specific fields and relations
        const listEntities: MarketNFTEntity[] = await MarketNFTApi.getAllApi_({
            fieldsForSelect: {},
            loadRelations: { smartUTxO_id: true }, // Load related data for smartUTxO_id
        });

        const listTokensWithMetadata: MarketNFTEntityWithMetadata[] = []; // Array to store MarketNFT entities with their metadata
        
        if (listEntities.length === 0) return []; // If no entities are found, return an empty list

        // Map the fetched entities to create a list of tokens with their CS and TN
        const listTokens = listEntities.map((item) => {
            return { CS: item.sellingToken_CS, TN_Hex: item.sellingToken_TN }; // Create a token object with CS and TN
        });

        // Fetch metadata for the tokens
        const listMetadata = await TokenMetadataFrontEndApiCalls.get_Tokens_MetadataApi(listTokens);

        // Combine MarketNFT entities with their corresponding metadata
        for (const item of listEntities) {
            const metadata = listMetadata.find((x) => x.CS === item.sellingToken_CS && x.TN_Hex === item.sellingToken_TN);
            if (metadata !== undefined) {
                listTokensWithMetadata.push({ entity: item, metadata }); // Push the entity and metadata pair into the list
            }
        }

        return listTokensWithMetadata; // Return the combined list of entities with metadata
    };

    //--------------------------------------
    // Use the `useList` hook from SmartDB to handle pagination and loading of the MarketNFT list
    const { isLoadingList, isLoadedList, list, refreshList } = useList<MarketNFTEntityWithMetadata>({
        nameList: MarketNFTEntity.className(), // Name of the list is the class name of MarketNFTEntity
        loadList, // The function to load the list of MarketNFTs with metadata
    });
    // Sync the market data with the blockchain
    const handleBtnSync = async () => {
        if (marketAddress === undefined) return;
        setIsLoadingSync(true);
        try {
            // Sync the data and refresh the list
            await MarketNFTApi.syncWithAddressApi(MarketNFTEntity, marketAddress, true);
            refreshList();
            pushSucessNotification(`MarketNFT Sync`, 'Synchronization complete!', false);
        } catch (e) {
            console.error(e);
            pushWarningNotification(`MarketNFT Sync`, 'Synchronization Error' + e);
        }
        setIsLoadingSync(false);
    };

    return {
        isLoading,
        isLoadingSync,
        isLoadingList,
        isLoadedList,
        list,
        refreshList,
        isLoadingAnyTx,
        setIsLoadingAnyTx,
        handleBtnSync,
    };
};
