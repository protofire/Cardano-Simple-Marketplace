
import 'reflect-metadata';

import { asSmartDBEntity, BaseSmartDBEntity, Convertible, type CS, type TN } from 'smart-db';
import { type PaymentKeyHash } from 'lucid-cardano';
import { mintingPolicyID_TN } from '../../Commons/Constants/onchain';

// The class is decorated with @asSmartDBEntity, indicating it's part of the Smart DB framework and can be used as an entity.
@asSmartDBEntity()
export class MarketNFTEntity extends BaseSmartDBEntity {
    // Static properties for API routing and class name.
    protected static _apiRoute: string = 'marketnft';
    protected static _className: string = 'MarketNFT';
    protected static _plutusDataIsSubType = false;

    // #region fields

    // The _is_NET_id_Unique flag indicates whether the NFT ID is unique in the datum (for NFTs) or if it's a fungible token (FT).
    protected static _is_NET_id_Unique = false;

    // The token name for the minting policy.
    _NET_id_TN: string = mintingPolicyID_TN;

    // The seller's payment public key hash, mapped to the datum.
    @Convertible({ isForDatum: true })
    sellerPaymentPKH!: PaymentKeyHash;

    // The policy ID of the asset being sold, mapped to the datum.
    @Convertible({ isForDatum: true })
    policyID_CS!: CS;

    // The selling token's asset class, mapped to the datum.
    @Convertible({ isForDatum: true })
    sellingToken_CS!: CS;

    // The token name of the selling token, mapped to the datum.
    @Convertible({ isForDatum: true })
    sellingToken_TN!: TN;

    // The price of the asset being sold, mapped to the datum.
    @Convertible({ isForDatum: true })
    priceOfAsset!: bigint;

    // The minimum ADA required for the transaction, mapped to the datum.
    @Convertible({ isForDatum: true })
    minADA!: bigint;

    // #endregion fields

    // #region db

    // The default fields to be set when they are undefined (currently empty).
    public static defaultFieldsWhenUndefined: Record<string, boolean> = {};

    // Fields that are always included in select queries for this entity, overriding the parent class's default fields.
    public static alwaysFieldsForSelect: Record<string, boolean> = {
        ...super.alwaysFieldsForSelect,

        sellerPaymentPKH: true,
        policyID_CS: true,
        sellingToken_CS: true,
        sellingToken_TN: true,
        priceOfAsset: true,
        minADA: true,
    };

    // #endregion db
}

