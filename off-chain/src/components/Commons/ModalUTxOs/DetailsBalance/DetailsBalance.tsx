import { UTxO } from 'lucid-cardano';
import { useDetailsBalance } from './useDetailsBalance';
import styles from './DetailsBalance.module.scss';
import {
    CopyButton,
    formatCurrencySymbol,
    formatTokenAmount,
    formatTokenNameHexToStr,
    getUrlForImage,
    hexToStr,
    isValidUrl,
    OpenInNewTabButton,
    TOKEN_ICON_GENERIC,
} from 'smart-db';
import LoaderButton from '../../LoaderButton/LoaderButton';
import Image from 'next/image';

// Component to display details of the user's balance
export const DetailsBalance = ({ uTxOs }: { uTxOs: UTxO[] }) => {
    //--------------------------------------
    // Destructure relevant data and actions from useDetailsBalance custom hook
    const { appStore, isLoadingDetails, isLoadedDetails, current } = useDetailsBalance({ uTxOs });
    //--------------------------------------

    // Render loader if details are still loading
    return isLoadingDetails ? (
        <div className={styles.loadingDiv}>{<LoaderButton />}</div>
    ) : (
        <>
            {/* If details are loaded and current data is available, render the table */}
            {isLoadedDetails === true && current !== undefined ? (
                <>
                    <table className={styles.tableModal}>
                        <thead className={styles.tableHeader}>
                            <tr>
                                <th>Currency Symbol</th>
                                <th>Amount</th>
                            </tr>
                        </thead>

                        {/* Scrollable table body containing balance details */}
                        <div className={styles.scrollableTable}>
                            <tbody>
                                {/* Iterate over each asset in the current data */}
                                {current.map((asset, index) => (
                                    <tr key={index}>
                                        <td>
                                            <div className={styles.itemWidthIcons}>
                                                {/* Display formatted currency symbol */}
                                                {formatCurrencySymbol(asset.CS)}
                                                {/* Copy button for currency symbol */}
                                                <CopyButton content={asset.CS} />
                                                {/* Button to open currency symbol in a new tab if site settings are available */}
                                                <OpenInNewTabButton
                                                    url={
                                                        appStore.siteSettings !== undefined
                                                            ? `${appStore.siteSettings.getblockfrost_url_explorer_policy(asset.CS)}`
                                                            : ``
                                                    }
                                                />
                                            </div>
                                        </td>

                                        <td>
                                            {/* Display asset logo, using a generic icon if no valid URL is found */}
                                            <div className={styles.logo}>
                                                <div className={styles.image}>
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
                                                            className={styles.tokenImage}
                                                        />
                                                    )}
                                                </div>
                                            </div>

                                            {/* Format and display the token amount */}
                                            {formatTokenAmount(
                                                asset.amount,
                                                asset.CS,
                                                asset.TN_Hex,
                                                asset.decimals,
                                                true,
                                                2
                                            )}
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

