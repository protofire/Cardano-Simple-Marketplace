import { UTxO } from 'lucid-cardano';
import { useDetailsUTxOs } from './useDetailsUTxO';
import styles from './DetailsUTxO.module.scss';
import LoaderButton from '../../LoaderButton/LoaderButton';
import {
    CopyButton,
    formatTokenAmount,
    formatTokenNameHexToStr,
    formatUTxO,
    getUrlForImage,
    hexToStr,
    isValidUrl,
    OpenInNewTabButton,
    TOKEN_ICON_GENERIC,
    Token_With_Metadata_And_Amount,
} from 'smart-db';
import Image from 'next/image';

// Component to display detailed information about a list of UTxOs
export const DetailsUTxOs = ({ uTxOs }: { uTxOs: UTxO[] }) => {
    //--------------------------------------
    // Retrieve various states and data from the useDetailsUTxOs custom hook
    const { appStore, isLoadingList, uTxOsWithDetails } = useDetailsUTxOs({ uTxOs });
    //--------------------------------------

    // Display a loading indicator if data is still loading
    return isLoadingList ? (
        <div className={styles.loadingDiv}>{<LoaderButton />}</div>
    ) : (
        <>
            {/* Main table structure for displaying UTxO details */}
            <table className={styles.tableModal}>
                <thead className={styles.tableHeader}>
                    <tr>
                        <th>Tx Hash # Output Index</th>
                        <th>Datum</th>
                        <th>Assets</th>
                        <th>Script Ref</th>
                    </tr>
                </thead>

                {/* Table body within a scrollable container for long lists */}
                <div className={styles.scrollableTable}>
                    <tbody>
                        {/* Map over UTxOs with their detailed information */}
                        {uTxOsWithDetails.map((utxo, index) => (
                            <tr key={index}>
                                {/* Display Tx Hash and Output Index with icons for copying and opening in new tab */}
                                <td>
                                    <div className={styles.itemWidthIcons}>
                                        {formatUTxO(utxo.txHash, utxo.outputIndex)}
                                        <CopyButton content={utxo.txHash + '#' + utxo.outputIndex} />
                                        <OpenInNewTabButton
                                            url={
                                                appStore.siteSettings !== undefined
                                                    ? `${appStore.siteSettings.getblockfrost_url_explorer_utxo(utxo.txHash + '#' + utxo.outputIndex)}`
                                                    : ``
                                            }
                                        />
                                    </div>
                                </td>

                                {/* Nested table to display details for each asset in the UTxO */}
                                <td>
                                    <table className="ResetTable">
                                        <tbody>
                                            {utxo.assetWithDetails.map((asset: Token_With_Metadata_And_Amount, index: number) => (
                                                <tr key={index}>
                                                    <td>
                                                        {/* Display token image if URL is valid, otherwise a generic token icon */}
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
                                                        {/* Format and display the token amount with relevant decimals */}
                                                        {formatTokenAmount(asset.amount, asset.CS, asset.TN_Hex, asset.decimals, true, 2)}
                                                    </td>
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </td>

                                {/* Display a script reference if available */}
                                <td>{utxo.hasScriptRef}</td>
                            </tr>
                        ))}
                    </tbody>
                </div>
            </table>
        </>
    );
    //--------------------------------------
};
