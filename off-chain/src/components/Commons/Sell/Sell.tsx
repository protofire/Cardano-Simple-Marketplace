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
    //--------------------------------------
    const { appStore, isRefreshing, isLoadingDetails, isLoadedDetails, current, refreshDetails } = useDetailsBalance({ uTxOs });
    //--------------------------------------
    return isLoadingDetails ? (
        <div className="">{<LoaderButton />}</div>
    ) : (
        <>
            {isLoadedDetails === true && current !== undefined ? (
                <>
                    <table className="">
                        <thead className="">
                            <tr>
                                <th>Currency Symbol</th>
                                <th>Amount</th>
                            </tr>
                        </thead>
                        <div className="">
                            <tbody>
                                {current.map((asset, index) => (
                                    <tr key={index}>
                                        <td>
                                            <div className="">
                                                {formatCurrencySymbol(asset.CS)}
                                                <CopyButton content={asset.CS} />
                                                <OpenInNewTabButton
                                                    url={appStore.siteSettings !== undefined ? `${appStore.siteSettings.getblockfrost_url_explorer_policy(asset.CS)}` : ``}
                                                />
                                            </div>
                                        </td>
                                        <td>
                                            <div className="">
                                                <div className="">
                                                    {isValidUrl(asset.image) ? (
                                                        <Image
                                                            src={getUrlForImage(asset.image!)}
                                                            width="100%"
                                                            height="100%"
                                                            layout="responsive"
                                                            objectFit="contain"
                                                            alt={`logo-${hexToStr(asset.TN_Hex)}`}
                                                            style={{ borderRadius: '50%' }}
                                                        />
                                                    ) : (
                                                        <Image
                                                            src={TOKEN_ICON_GENERIC.toString()}
                                                            width="100%"
                                                            height="100%"
                                                            layout="responsive"
                                                            objectFit="contain"
                                                            alt={formatTokenNameHexToStr(asset.TN_Hex)}
                                                            title={formatTokenNameHexToStr(asset.TN_Hex)}
                                                            className=""
                                                        />
                                                    )}
                                                </div>
                                            </div>
                                            {formatTokenAmount(asset.amount, asset.CS, asset.TN_Hex, asset.decimals, true, 2)}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </div>
                    </table>
                </>
            ) : null}
        </>
    );
    //--------------------------------------
};
