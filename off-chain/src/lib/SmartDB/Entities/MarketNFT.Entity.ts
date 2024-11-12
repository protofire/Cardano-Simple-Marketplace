import 'reflect-metadata';

import { asSmartDBEntity, BaseSmartDBEntity, Convertible, type CS, type TN } from 'smart-db';

import { type PaymentKeyHash } from 'lucid-cardano';
import { mintingPolicyID_TN } from '../../Commons/Constants/onchain';

@asSmartDBEntity()
export class MarketNFTEntity extends BaseSmartDBEntity {
    protected static _apiRoute: string = 'marketnft';
    protected static _className: string = 'MarketNFT';
    protected static _plutusDataIsSubType = false;

    // #region fields

    // si tiene id NFT en el datum o si es un FT
    protected static _is_NET_id_Unique = false;
    _NET_id_TN: string = mintingPolicyID_TN;

    @Convertible({ isForDatum: true })
    sellerPaymentPKH!: PaymentKeyHash;

    @Convertible({ isForDatum: true })
    policyID_CS!: CS;

    @Convertible({ isForDatum: true })
    sellingToken_CS!: CS;

    @Convertible({ isForDatum: true })
    sellingToken_TN!: TN;

    @Convertible({ isForDatum: true })
    priceOfAsset!: bigint;

    @Convertible({ isForDatum: true })
    minADA!: bigint;

    // #endregion fields

    // #region db

    public static defaultFieldsWhenUndefined: Record<string, boolean> = {};

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
