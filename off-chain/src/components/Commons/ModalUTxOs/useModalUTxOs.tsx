
import { UTxO } from 'lucid-cardano';
import { useEffect, useState } from 'react';
import { useWalletStore, useAppStore, useList } from 'smart-db';

export const useModalUTxOsAndBalance = ({ address, uTxOs, showBalance = false }: { address: string; uTxOs?: UTxO[]; showBalance?: boolean }) => {
    //--------------------------------------
    // State to track if data is refreshing
    const [isRefreshing, setIsRefreshing] = useState(true);
    
    // Once the component is mounted, set isRefreshing to false to stop the loading state
    useEffect(() => {
        setIsRefreshing(false);
    }, []);
    //--------------------------------------

    // Access the wallet store and app store for fetching necessary data
    const walletStore = useWalletStore();
    const appStore = useAppStore();
    //--------------------------------------

    // Function to load UTxOs based on the provided address or provided UTxOs prop
    const loadList = async () => {
        let fetchedUTxOs: UTxO[] = [];
        
        // If no UTxOs are passed as props, fetch the UTxOs for the address
        if (uTxOs === undefined) {
            // Get the Lucid instance (Cardano SDK) to interact with the blockchain
            const lucid = (await walletStore.getLucid()) || (await walletStore.getLucidForUseAsUtils());
            
            // If Lucid is available, fetch the UTxOs at the given address
            if (lucid) {
                fetchedUTxOs = await lucid.utxosAt(address);
            }
        } else {
            // Use the passed UTxOs directly if available
            fetchedUTxOs = uTxOs;
        }
        return fetchedUTxOs;
    };
    //--------------------------------------

    // State to manage if the modal is open or not
    const [isOpen, setIsOpen] = useState(false);
    
    // Function to check if the modal is open, triggering the UTxOs fetch when opened
    function checkDependencies() {
        let doLoadList = false;
        
        // Only trigger loading the list when the modal is open
        if (isOpen === true) {
            doLoadList = true;
        }
        return doLoadList;
    }
    //--------------------------------------

    // Use the custom hook `useList` to fetch and manage the UTxOs list
    const { isLoadingList, list } = useList<UTxO>({
        nameList: 'UTxOs',
        loadList, // The function that fetches the UTxOs
        checkDependencies, // Dependencies to trigger the list load
        dependencies: [isOpen], // Re-fetch UTxOs when the modal is opened
    });
    //--------------------------------------

    // State to toggle between balance and UTxOs view
    const [showBalance_, setShowBalance] = useState(showBalance);

    // Effect hook to update the `showBalance_` state when the prop changes
    useEffect(() => {
        setShowBalance(showBalance);
    }, []);
    //--------------------------------------

    // Functions to open and close the modal
    const handleOpen = () => {
        setIsOpen(true); // Set modal to open
    };
    const handleClose = () => setIsOpen(false); // Set modal to close
    //--------------------------------------

    // Return values to be used in the component
    return {
        appStore, // Provides access to the app’s global settings (e.g., Blockfrost URLs)
        isRefreshing, // Indicates if data is still being fetched
        isLoadingList, // Loading state for the UTxOs list
        list, // The list of UTxOs fetched for the address
        showBalance_, // Boolean indicating whether to show balance or UTxOs
        isOpen, // Boolean indicating if the modal is open
        handleOpen, // Function to open the modal
        handleClose, // Function to close the modal
        setShowBalance, // Function to toggle between balance and UTxOs view
    };
    //--------------------------------------
};

