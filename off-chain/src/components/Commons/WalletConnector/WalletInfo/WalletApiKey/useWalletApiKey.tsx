import { useSession } from 'next-auth/react';
import { useEffect, useState } from 'react';
import { AuthApi, Credentials, pushSucessNotification, useDetails, useWalletStore } from 'smart-db';

export const useWalletApiKey = () => {
    //--------------------------------------
    // State to track if data is still refreshing
    const [isRefreshing, setIsRefreshing] = useState(true);
    
    // Set isRefreshing to false once the component is mounted to stop the loading state
    useEffect(() => {
        setIsRefreshing(false);
    }, []);
    //--------------------------------------

    // Function to load API token details using wallet information
    const loadDetails = async () => {
        let token: string | undefined;
        
        // Get the Lucid instance (Cardano SDK) for blockchain interaction
        const lucid = await walletStore.getLucid();
        
        // Only proceed if the user is authenticated and the Lucid instance is available
        if (status === 'authenticated' && lucid !== undefined && session && session.user && apiToken === '') {
            // Prepare the credentials object with user data from the session
            const credentials: Credentials = {
                address: session.user.address,
                walletNameOrSeedOrKey: session.user.walletNameOrSeedOrKey,
                useBlockfrostToSubmit: session.user.useBlockfrostToSubmit ? 'true' : 'false',
                isWalletFromSeed: session.user.isWalletFromSeed ? 'true' : 'false',
                isWalletFromKey: session.user.isWalletFromKey ? 'true' : 'false',
            };
            
            // Fetch the API token using the credentials and the Lucid instance
            token = await AuthApi.generateAuthTokensApi(lucid, credentials, true);
        }
        return token;
    };
    //--------------------------------------

    // Function to check if the API token details should be loaded based on modal open state
    function checkDependencies() {
        let doLoadDetails = false;
        
        // Trigger loading the details when the modal is open
        if (isOpen === true) {
            doLoadDetails = true;
        }
        return doLoadDetails;
    }
    //--------------------------------------

    // State to manage if the modal displaying the API token details is open or closed
    const [isOpen, setIsOpen] = useState(false);
    //--------------------------------------

    // Fetching API token details using the useDetails hook (custom hook)
    const { isLoadingDetails, isLoadedDetails, current } = useDetails<string>({
        nameDetails: 'Api Token', // Name of the details being fetched
        loadDetails, // Function to load the details (fetch API token)
        checkDependencies, // Dependencies to trigger the load function
        dependencies: [isOpen], // Only fetch data when the modal is open
    });
    //--------------------------------------

    // Functions to handle opening and closing the modal
    const handleOpen = () => {
        setIsOpen(true); // Open the modal
    };
    const handleClose = () => setIsOpen(false); // Close the modal
    //--------------------------------------

    // Function to copy the API token to the clipboard and show a success notification
    const handleCopy = () => {
        navigator.clipboard.writeText(apiToken); // Copy the token to clipboard
        pushSucessNotification(`SmartDB`, `Copied to clipboard!`, false); // Show success notification
    };
    //--------------------------------------

    // Access session data to check if the user is authenticated
    const { data: session, status } = useSession();
    //--------------------------------------

    // State to hold the fetched API token and the ready state for the wallet
    const [apiToken, setApiToken] = useState('');
    const [isReady, setIsReady] = useState(false);
    //--------------------------------------

    // Access the wallet store to check wallet connection status
    const walletStore = useWalletStore();
    //--------------------------------------

    // Effect to update the `apiToken` state when `current` API token data is available
    useEffect(() => {
        if (current !== undefined) {
            setApiToken(current); // Set the API token from the details fetched
        }
    }, [current]);
    //--------------------------------------

    // Effect to check if the user is authenticated and the wallet is connected
    useEffect(() => {
        if (status === 'authenticated' && walletStore.isConnected === true) {
            setIsReady(true); // Set `isReady` to true when the user is authenticated and the wallet is connected
        }
    }, [status, walletStore.isConnected]);
    //--------------------------------------

    // Return necessary states and functions for use in the component
    return {
        isRefreshing, // Indicates if data is still refreshing
        walletStore, // Access to wallet store (for checking connection status)
        isReady, // Indicates if the wallet is ready (authenticated and connected)
        isLoadedDetails, // Indicates if the API token details are loaded
        isLoadingDetails, // Indicates if the API token details are still being loaded
        apiToken, // The fetched API token
        isOpen, // Indicates if the modal is open or closed
        handleOpen, // Function to open the modal
        handleClose, // Function to close the modal
        handleCopy, // Function to copy the API token to clipboard
    };
    //--------------------------------------
};

