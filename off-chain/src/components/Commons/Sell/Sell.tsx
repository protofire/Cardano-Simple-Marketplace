import { UTxO } from 'lucid-cardano';
import { useDetailsBalance } from '@/components/Commons/ModalUTxOs/DetailsBalance/useDetailsBalance';
import {
  CopyButton,
  formatCurrencySymbol,
  formatTokenAmount,
  formatTokenNameHexToStr,
  getUrlForImage,
  hexToStr,
  isValidUrl,
  OpenInNewTabButton,
  toJson,
  TOKEN_ICON_GENERIC,
} from 'smart-db';
import LoaderButton from '@/components/Commons/LoaderButton/LoaderButton';
import Image from 'next/image';

export const Sell = ({ uTxOs }: { uTxOs: UTxO[] }) => {
  const { appStore, isRefreshing, isLoadingDetails, isLoadedDetails, current, refreshDetails } = useDetailsBalance({ uTxOs });

  return isLoadingDetails ? (
    <div className="flex justify-center items-center h-48">
      <LoaderButton />
    </div>
  ) : (
    <>
      {isLoadedDetails && current !== undefined ? (
        <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4">
          {current.map((asset, index) => (
            <div key={index} className="bg-zinc-800 rounded-lg p-4 flex flex-col items-center text-white shadow-lg">
              <div className="w-24 h-24 mb-4">
                {isValidUrl(asset.image) ? (
                  <img src={getUrlForImage(asset.image!)} alt={`logo-${hexToStr(asset.TN_Hex)}`} className="w-full h-full object-contain rounded-full" />
                ) : (
                  <img src={TOKEN_ICON_GENERIC.toString()} alt={formatTokenNameHexToStr(asset.TN_Hex)} className="w-full h-full object-contain rounded-full" />
                )}
              </div>
              <p className="text-center text-lg font-semibold mb-2">{formatTokenNameHexToStr(asset.TN_Hex)}</p>
              <div className="flex items-center w-full mt-2">
                <input
                  type="text"
                  placeholder="Enter price"
                  className="bg-zinc-700 text-white rounded-lg px-3 py-1 w-2/3 focus:outline-none focus:ring focus:ring-green-500"
                />
                <button
                  className="bg-green-600 text-white font-semibold py-1 px-4 rounded-lg ml-2 transition hover:bg-green-700 active:scale-95"
                  onClick={() => {
                    /* Logic for selling token here */
                  }}
                >
                  Sell
                </button>
              </div>
            </div>
          ))}
        </div>
      ) : null}
    </>
  );
};
