import { AppStateContext } from '@/pages/_app';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities/MarketNFT.Entity';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd/MarketNFT.FrontEnd.Api.Calls';
import { useContext, useState } from 'react';
import { pushSucessNotification, pushWarningNotification, TokenMetadataEntity, TokenMetadataFrontEndApiCalls, useList, useWalletStore } from 'smart-db';

export interface BuyProps {
    isWalletConnectorModalOpen: boolean;
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>;
}

//TODO: explicar que este type es para manejar la lista de MarketNFTs con metadata, se levanta primero la lista y lego se piden los meta de cada token y se agregan en lalista
type MarketNFTEntityWithMetadata = {
    entity: MarketNFTEntity;
    metadata: TokenMetadataEntity;
};

export const useBuy = ({ isWalletConnectorModalOpen, setIsWalletConnectorModalOpen }: BuyProps) => {
    //----------------------------------------------------------------------------
    const walletStore = useWalletStore(); // Access the wallet store
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext); // Access app state
    const { marketAddress } = appState; // Extract relevant data from app state
    //----------------------------------------------------------------------------
    // State variables to manage loading states and transaction modal
    const [isLoading, setIsLoading] = useState(false);
    const [isLoadingSync, setIsLoadingSync] = useState(false);
    const [isLoadingAnyTx, setIsLoadingAnyTx] = useState<string | undefined>(undefined);
    //----------------------------------------------------------------------------
    //
    // Function to load list of MarketNFTs with metadata
    const loadList = async () => {
        const listEntities: MarketNFTEntity[] = await MarketNFTApi.getAllApi_({
            fieldsForSelect: {},
            loadRelations: { smartUTxO_id: true },
        });
        const listTokensWithMetadata: MarketNFTEntityWithMetadata[] = [];
        if (listEntities.length === 0) return [];
        const listTokens = listEntities.map((item) => {
            return { CS: item.sellingToken_CS, TN_Hex: item.sellingToken_TN };
        });
        // Fetch metadata for the tokens
        const listMetadata = await TokenMetadataFrontEndApiCalls.get_Tokens_MetadataApi(listTokens);
        // Combine MarketNFT entities with their metadata
        for (const item of listEntities) {
            const metadata = listMetadata.find((x) => x.CS === item.sellingToken_CS && x.TN_Hex === item.sellingToken_TN);
            if (metadata !== undefined) {
                listTokensWithMetadata.push({ entity: item, metadata });
            }
        }
        return listTokensWithMetadata;
    };
    //--------------------------------------
    // Use the `useList` hook from helpers in SmartDB to handle pagination and loading of the MarketNFT list
    const { isLoadingList, isLoadedList, list, refreshList } = useList<MarketNFTEntityWithMetadata>({
        nameList: MarketNFTEntity.className(),
        loadList,
    });
    //--------------------------------------

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
