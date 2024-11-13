import { useState } from 'react';

interface UseModalTransactionReturn {
  isModalOpen: boolean;
  setIsModalOpen: (open: boolean) => void;
  copied: boolean;
  handleCopyToClipboard: (text: string) => void;
}

export function useModalTransaction(): UseModalTransactionReturn {
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [copied, setCopied] = useState(false);

  const handleCopyToClipboard = (text: string) => {
    navigator.clipboard
      .writeText(text || '')
      .then(() => {
        setCopied(true);
        setTimeout(() => setCopied(false), 1000);
      })
      .catch((err) => {
        console.error('Error copying text:', err);
      });
  };

  return { isModalOpen, setIsModalOpen, copied, handleCopyToClipboard };
}
