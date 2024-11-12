import React from 'react';
import Modal from 'react-modal';
import styles from './ModalTransaction.module.scss';
import { useState } from 'react';

interface ModalTransactionProps {
  isOpen: boolean;
  onRequestClose: () => void;
  txMessage: string;
  txHash: string;
  txConfirmed: boolean;
  isTxError: boolean;
  closeButtonText?: string;
}

const ModalTransaction: React.FC<ModalTransactionProps> = ({ isOpen, onRequestClose, txMessage, txHash, txConfirmed, isTxError, closeButtonText = 'Close' }) => {
  const [copied, setCopied] = useState(false);
  const handleCopyToClipboard = (text: string) => {
    navigator.clipboard
      .writeText(text == 'undefined' ? text : '')
      .then(() => {
        setCopied(true); // Cambiar el estado a 'true' cuando se copie
        setTimeout(() => {
          setCopied(false); // Resetear el estado después de 2 segundos
        }, 1000);
      })
      .catch((err) => {
        console.error('Error copying text: ', err);
      });
  };
  return (
    <Modal isOpen={isOpen} onRequestClose={onRequestClose} contentLabel="Transaction Status" className={styles.modal} overlayClassName={styles.overlay}>
      <h2 className={styles.title}>Transaction Status</h2>
      <div className="mt-4">
        <textarea value={txMessage} readOnly className={styles.textarea}></textarea>
      </div>
      <div className="mt-4 flex items-center">
        <span className="text-gray-700 font-semibold mr-2">Tx Hash:</span>
        <div
          className={`${styles.txHashContainer} ${copied ? (txHash ? styles.copied : styles.copiedError) : ''} `}
          onClick={() => handleCopyToClipboard(txHash)}
          style={{ cursor: 'pointer' }}
        >
          {txHash}{' '}
          {!copied ? (
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" className="bi bi-clipboard2 inline" viewBox="0 0 16 16">
              <path d="M3.5 2a.5.5 0 0 0-.5.5v12a.5.5 0 0 0 .5.5h9a.5.5 0 0 0 .5-.5v-12a.5.5 0 0 0-.5-.5H12a.5.5 0 0 1 0-1h.5A1.5 1.5 0 0 1 14 2.5v12a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 14.5v-12A1.5 1.5 0 0 1 3.5 1H4a.5.5 0 0 1 0 1z" />
              <path d="M10 .5a.5.5 0 0 0-.5-.5h-3a.5.5 0 0 0-.5.5.5.5 0 0 1-.5.5.5.5 0 0 0-.5.5V2a.5.5 0 0 0 .5.5h5A.5.5 0 0 0 11 2v-.5a.5.5 0 0 0-.5-.5.5.5 0 0 1-.5-.5" />
            </svg>
          ) : txHash ? (
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" className="bi bi-clipboard2-check-fill inline" viewBox="0 0 16 16">
              <path d="M10 .5a.5.5 0 0 0-.5-.5h-3a.5.5 0 0 0-.5.5.5.5 0 0 1-.5.5.5.5 0 0 0-.5.5V2a.5.5 0 0 0 .5.5h5A.5.5 0 0 0 11 2v-.5a.5.5 0 0 0-.5-.5.5.5 0 0 1-.5-.5" />
              <path d="M4.085 1H3.5A1.5 1.5 0 0 0 2 2.5v12A1.5 1.5 0 0 0 3.5 16h9a1.5 1.5 0 0 0 1.5-1.5v-12A1.5 1.5 0 0 0 12.5 1h-.585q.084.236.085.5V2a1.5 1.5 0 0 1-1.5 1.5h-5A1.5 1.5 0 0 1 4 2v-.5q.001-.264.085-.5m6.769 6.854-3 3a.5.5 0 0 1-.708 0l-1.5-1.5a.5.5 0 1 1 .708-.708L7.5 9.793l2.646-2.647a.5.5 0 0 1 .708.708" />
            </svg>
          ) : (
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" className="inline bi bi-clipboard2-x" viewBox="0 0 16 16">
              <path d="M9.5 0a.5.5 0 0 1 .5.5.5.5 0 0 0 .5.5.5.5 0 0 1 .5.5V2a.5.5 0 0 1-.5.5h-5A.5.5 0 0 1 5 2v-.5a.5.5 0 0 1 .5-.5.5.5 0 0 0 .5-.5.5.5 0 0 1 .5-.5z" />
              <path d="M3 2.5a.5.5 0 0 1 .5-.5H4a.5.5 0 0 0 0-1h-.5A1.5 1.5 0 0 0 2 2.5v12A1.5 1.5 0 0 0 3.5 16h9a1.5 1.5 0 0 0 1.5-1.5v-12A1.5 1.5 0 0 0 12.5 1H12a.5.5 0 0 0 0 1h.5a.5.5 0 0 1 .5.5v12a.5.5 0 0 1-.5.5h-9a.5.5 0 0 1-.5-.5z" />
              <path d="M8 8.293 6.854 7.146a.5.5 0 1 0-.708.708L7.293 9l-1.147 1.146a.5.5 0 0 0 .708.708L8 9.707l1.146 1.147a.5.5 0 0 0 .708-.708L8.707 9l1.147-1.146a.5.5 0 0 0-.708-.708z" />
            </svg>
          )}
        </div>
      </div>
      <div className="mt-2 flex items-center">
        <span className="text-gray-700 font-semibold mr-2">Status:</span>
        <div className={`${styles.status} ${txConfirmed ? styles.confirmed : isTxError ? styles.error : styles.waiting}`}>
          {txConfirmed ? 'Confirmed' : isTxError ? 'Error' : 'Waiting...'}
        </div>
      </div>
      <button onClick={onRequestClose} className={styles.closeButton}>
        {closeButtonText}
      </button>
    </Modal>
  );
};

export default ModalTransaction;
