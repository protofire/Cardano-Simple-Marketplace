import { useContext, useState } from 'react';
import { hexToStr, useWalletStore, Token_With_Metadata_And_Amount, Token, BaseSmartDBFrontEndBtnHandlers } from 'smart-db';
import { SellMarketNFTTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { mintingPolicyID_TN } from '@example/src/lib/Commons/Constants/onchain';
import { MarketNFTEntity } from '@example/src/lib/SmartDB/Entities';
import { MarketNFTApi } from '@example/src/lib/SmartDB/FrontEnd';
import { AppStateContext } from '@example/src/pages/_app';

export interface TokenSellProps {
    tokenToSell: Token_With_Metadata_And_Amount;
    isWalletConnectorModalOpen: boolean;
    setIsWalletConnectorModalOpen: React.Dispatch<React.SetStateAction<boolean>>;
    isLoadingAnyTx: string | undefined; // Pass down from parent
    setIsLoadingAnyTx: React.Dispatch<React.SetStateAction<string | undefined>>; // Pass down from parent
}

export const useTokenSell = ({ tokenToSell, isLoadingAnyTx, setIsLoadingAnyTx }: TokenSellProps) => {
    const walletStore = useWalletStore(); // Access the wallet store
    //----------------------------------------------------------------------------
    const { appState, setAppState } = useContext(AppStateContext); // Access app state
    const { marketScript, marketAddress, mintingPolicyIDScript, mintingPolicyID_CS } = appState; // Extract relevant data from app state
    //----------------------------------------------------------------------------
    const [isTxModalOpen, setIsTxModalOpen] = useState(false); // State for showing transaction modal
    const [txHash, setTxHash] = useState<string>(); // Transaction hash state
    const [isTxError, setIsTxError] = useState(false); // Error state for transaction
    const [txMessage, setTxMessage] = useState(''); // Message related to transaction
    const [txConfirmed, setTxConfirmed] = useState(false); // Confirmation state for transaction
    //----------------------------------------------------------------------------
    const [inputValue, setInputValue] = useState<string>(''); // Single value for token price
    const [isLoadingTxSell, setIsLoadingTxSell] = useState<boolean>(false); // Single boolean for loading state
    const [isValidInput, setIsValidInput] = useState<boolean>(false); // Single boolean for validating the imput
    //----------------------------------------------------------------------------

    // Function to handle the sell transaction for a specific asset
    const handleBtnSellTx = async () => {
        if (walletStore.isConnected !== true) return; // Ensure wallet is connected
        if (marketAddress === undefined || marketScript === undefined || mintingPolicyIDScript === undefined || mintingPolicyID_CS === undefined) {
            return; // Ensure all required values are available before proceeding
        }
        if (!(isLoadingAnyTx === undefined || isLoadingAnyTx == tokenToSell.CS + tokenToSell.TN_Hex)) return;

        setIsTxModalOpen(true); // Open transaction modal

        if (isLoadingTxSell) return;

        setIsLoadingAnyTx(tokenToSell.CS + tokenToSell.TN_Hex);
        setIsLoadingTxSell(true);

        setTxConfirmed(false);
        const token_TN = hexToStr(tokenToSell.TN_Hex); // Convert token name from hex to string
        const token_CS = tokenToSell.CS; // Get the token's CS value

        try {
            setTxHash(undefined);
            setIsTxError(false);
            setTxMessage('Creating Transaction...'); // Show loading message

            // Set up parameters for the transaction
            const txParams: SellMarketNFTTxParams = {
                token_TN,
                token_CS,
                datumID_CS: mintingPolicyID_CS,
                datumID_TN: mintingPolicyID_TN,
                validatorAddress: marketAddress,
                mintingPolicyID: mintingPolicyIDScript,
                validatorMarket: marketScript,
                priceOfAsset: BigInt(inputValue), // Use the input value as the asset price
            };

            // Call the transaction handler to process the transaction
            const result = await BaseSmartDBFrontEndBtnHandlers.handleBtnDoTransactionV1(
                MarketNFTEntity,
                'Creating MarketNFT...',
                'Create Tx',
                setTxMessage,
                setTxHash,
                walletStore,
                txParams,
                MarketNFTApi.callGenericTxApi_.bind(MarketNFTApi, 'sell-nft-tx')
            );

            if (result === false) {
                throw 'There was an error in the transaction'; // Handle failure
            }

            setTxMessage('Transaction has been confirmed. Refreshing data...');
            setTxConfirmed(result); // Set transaction as confirmed
        } catch (e) {
            console.error(e);
            setTxHash(undefined);
            setIsTxError(true); // Set error flag if transaction fails
        }

        setIsLoadingAnyTx(undefined);
        setIsLoadingTxSell(false);
    };

    const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        setInputValue(e.target.value);
        setIsValidInput(!(isNaN(Number(e.target.value)) && Number(e.target.value) <= 0));
    };

    return {
        appState,
        walletStore,
        inputValue,
        isLoadingTxSell,
        isTxModalOpen,
        isValidInput,
        txHash,
        isTxError,
        txMessage,
        txConfirmed,
        handleBtnSellTx,
        handleInputChange,
        setIsTxModalOpen,
    };
};
