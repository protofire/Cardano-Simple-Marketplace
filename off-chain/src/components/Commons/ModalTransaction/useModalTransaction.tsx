import { useState } from 'react';

// Define the return type for the useModalTransaction hook
interface UseModalTransactionReturn {
  isModalOpen: boolean; // State indicating whether the modal is open
  setIsModalOpen: (open: boolean) => void; // Function to set modal open/closed
  copied: boolean; // State indicating if text has been copied to clipboard
  handleCopyToClipboard: (text: string) => void; // Function to copy text to clipboard
}

// Custom hook for managing modal transaction state and clipboard functionality
export function useModalTransaction(): UseModalTransactionReturn {
  // State for tracking modal visibility
  const [isModalOpen, setIsModalOpen] = useState(false);
  // State for tracking if text was successfully copied
  const [copied, setCopied] = useState(false);

  // Function to handle copying text to clipboard
  const handleCopyToClipboard = (text: string) => {
    navigator.clipboard
      .writeText(text || '') // Writes text to clipboard (defaults to empty string if text is falsy)
      .then(() => {
        setCopied(true); // Sets `copied` to true upon successful copy
        setTimeout(() => setCopied(false), 1000); // Resets `copied` to false after 1 second
      })
      .catch((err) => {
        console.error('Error copying text:', err); // Logs error if copying fails
      });
  };

  // Return the modal and clipboard states and their handlers
  return { isModalOpen, setIsModalOpen, copied, handleCopyToClipboard };
}

