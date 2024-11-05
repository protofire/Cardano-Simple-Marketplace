import { useContext, useState } from 'react';

import { ClaimDummyTxParams, CreateDummyTxParams, UpdateDummyTxParams } from '@example/src/lib/Commons/Constants/transactions';
import { DummyEntity } from '@example/src/lib/SmartDB/Entities/Dummy.Entity';
import { DummyApi } from '@example/src/lib/SmartDB/FrontEnd/Dummy.FrontEnd.Api.Calls';
import Modal from 'react-modal';
import {
  ADA_UI,
  BaseSmartDBFrontEndApiCalls,
  BaseSmartDBFrontEndBtnHandlers,
  CS,
  formatHash,
  formatTokenAmount,
  formatUTxO,
  LucidToolsFrontEnd,
  pushSucessNotification,
  pushWarningNotification,
  useTokensStore,
  useWalletStore,
} from 'smart-db';
import LoaderButton from '../../Commons/LoaderButton/LoaderButton';
import { Sell } from '@/components/Commons/Sell/Sell';
import { DetailsBalance } from '@example/src/components/Commons/ModalUTxOs/DetailsBalance/DetailsBalance';
import styles from './Home.module.scss';
import { HiUserCircle } from 'react-icons/hi';
import { IoReloadCircleSharp } from 'react-icons/io5';
import WalletSelectorModal, { Wallet } from '../../Commons/WalletConnector/WalletSelectorModal';
import WalletConnector from '../../Commons/WalletConnector/WalletConnector';

import { AppStateContext, MenuClass } from '@/pages/_app';

export default function Home() {
  const walletStore = useWalletStore();
  const tokensStore = useTokensStore();
  const { appState, setAppState } = useContext(AppStateContext);
  const [showWalletModal, setShowWalletModal] = useState(false);
  //--------------------------------------
  const [list, setList] = useState<DummyEntity[]>();
  const [selectedItem, setSelectedItem] = useState<DummyEntity>();
  //--------------------------------------
  const { wAddr, menuClass, marketAddress } = appState;
  //--------------------------------------
  const [isLoading, setIsLoading] = useState(false);
  const [isLoadingSync, setIsLoadingSync] = useState(false);
  const [isLoadingBalance, setIsLoadingBalance] = useState(false);
  const [isLoadingTxCreate, setIsLoadingTxCreate] = useState(false);
  const [isLoadingTxUpdate, setIsLoadingTxUpdate] = useState(false);
  const [isLoadingTxClaim, setIsLoadingTxClaim] = useState(false);
  //--------------------------------------
  const [isEditingValue, setIsEditingValue] = useState(false);
  const [editValue, setEditValue] = useState('');

  const [isTxModalOpen, setIsTxModalOpen] = useState(false);
  const [txMessage, setTxMessage] = useState('');
  const [txHash, setTxHash] = useState<string>();
  const [txConfirmed, setTxConfirmed] = useState(false);
  const [isTxError, setIsTxError] = useState(false);

  const handleBtnSync = async () => {
    //----------------------------
    if (appState.lucid === undefined) return;
    if (wAddr === undefined) return;
    //----------------------------
    setIsLoadingSync(true);
    //----------------------------
    try {
      //----------------------------
      await DummyApi.syncWithAddressApi(DummyEntity, marketAddress, true);
      const list: DummyEntity[] = await DummyApi.getAllApi_({
        fieldsForSelect: {},
        loadRelations: { smartUTxO_id: true },
      });
      setList(list);
      //----------------------------
      pushSucessNotification(`Dummy Sync`, 'Syncronization complete!', false);
      //----------------------------
    } catch (e) {
      console.error(e);
      pushWarningNotification(`Dummy Sync`, 'Syncronization Error' + e);
    }
    //----------------------------
    setIsLoadingSync(false);
  };
  const refreshWallet = async () => {
    setShowWalletModal(true);
  };

  const handleClickMenuClass = (v: MenuClass) => {
    setAppState({
      ...appState,
      menuClass: v,
    });

    console.log(v);
  };
  return (
    <main className="pt-24 px-4 sm:px-8 lg:px-16 bg-gray-100 min-h-screen">
      {/* USER LOGGED */}

      <header className="fixed top-0 left-0 w-full bg-gray-900 text-white flex justify-between items-center p-4 shadow-md z-10">
        {/* Botones de ContractType alineados a la izquierda */}
        <div className="flex space-x-4">
          <button
            onClick={() => handleClickMenuClass('Buy')}
            className={`${menuClass === 'Buy' ? 'bg-gradient-to-r from-green-400 to-green-600 text-white' : 'bg-gray-700 text-gray-300 hover:bg-gray-600'
              } font-semibold text-lg py-2 px-6 rounded-lg transition-all duration-200 ease-in-out transform active:scale-95`}
          >
            Buy
          </button>
          <button
            onClick={() => handleClickMenuClass('Sell')}
            className={`${menuClass === 'Sell' ? 'bg-gradient-to-r from-red-400 to-red-600 text-white' : 'bg-gray-700 text-gray-300 hover:bg-gray-600'
              } font-semibold text-lg py-2 px-6 rounded-lg transition-all duration-200 ease-in-out transform active:scale-95`}
          >
            Sell
          </button>
        </div>

        {/* Selector de Wallet alineado a la derecha */}
        <div className="flex items-center space-x-2">
          {<WalletConnector lucid={appState.lucid!} />}
        </div>
      </header>

      <body className="bg-gray-100 text-gray-900 font-sans p-4">
        <section className="bg-white rounded-lg shadow-md p-6 mb-6">
          <div className="text-lg font-semibold text-gray-800 mb-4">Sync DB with Blockchain</div>
          <button
            onClick={handleBtnSync}
            className="bg-green-500 hover:bg-green-600 text-white font-semibold py-2 px-4 rounded transition-colors duration-200"
            disabled={isLoadingSync}
          >
            Sync{' '}
            {isLoadingSync && (
              <>
                <LoaderButton />
              </>
            )}
          </button>
          <div className="mt-4 text-gray-700">
            Smart DB - Dummy Entity{' '}
            {isLoading && (
              <>
                <LoaderButton />
              </>
            )}
          </div>
        </section>

        <section className="bg-white rounded-lg shadow-md p-6">
          {menuClass === 'Buy' && <text> To be determinated </text> }
          {menuClass === 'Sell' && ( < Sell uTxOs={walletStore.uTxOsAtWallet} />)}
        </section>

        <Modal
          isOpen={isTxModalOpen}
          onRequestClose={() => setIsTxModalOpen(false)}
          contentLabel="Transaction Status"
          className="bg-white p-6 rounded-lg shadow-lg max-w-lg w-full"
          overlayClassName="fixed inset-0 bg-gray-800 bg-opacity-75 flex items-center justify-center"
        >
          <h2 className="text-xl font-semibold text-gray-800">Transaction Status</h2>
          <div className="mt-4">
            <textarea value={txMessage} readOnly className="w-full h-24 border border-gray-300 rounded p-2 resize-none text-gray-700"></textarea>
          </div>
          <div className="mt-4 flex items-center">
            <span className="text-gray-700 font-semibold mr-2">Tx Hash:</span>
            <div className="bg-gray-200 p-2 rounded text-gray-800">{txHash}</div>
          </div>
          <div className="mt-2 flex items-center">
            <span className="text-gray-700 font-semibold mr-2">Status:</span>
            <div
              className={`
        ${txConfirmed ? 'text-green-500' : isTxError ? 'text-red-500' : 'text-yellow-500'}
        font-semibold
      `}
            >
              {txConfirmed ? 'Confirmed' : isTxError ? 'Error' : 'Waiting...'}
            </div>
          </div>
          <button
            onClick={() => setIsTxModalOpen(false)}
            className="bg-blue-500 hover:bg-blue-600 text-white font-semibold py-2 px-4 rounded mt-4 transition-colors duration-200"
          >
            Close
          </button>
        </Modal>
      </body>
    </main>
  );
}
