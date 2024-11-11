import 'reflect-metadata';

import { asSmartDBEntity, BaseSmartDBEntity, Convertible, type CS, type TN } from 'smart-db';

import {type PaymentKeyHash } from 'lucid-cardano';

@asSmartDBEntity()
export class MarketNFTEntity extends BaseSmartDBEntity {
    protected static _apiRoute: string = 'marketnft';
    protected static _className: string = 'MarketNFT';

    // #region fields

    @Convertible({ isForDatum: true })
    sellerAddress!: PaymentKeyHash;

    @Convertible({ isForDatum: true })
    policyID_CS!: CS;

    @Convertible({ isForDatum: true })
    sellingToken_CS!: CS;

    @Convertible({ isForDatum: true })
    sellingToken_TN!: TN;

    @Convertible({ isForDatum: true })
    priceOfAsset!: BigInt;

    @Convertible({ isForDatum: true })
    minADA!: BigInt;
    // #endregion fields

    // #region db

    public static defaultFieldsWhenUndefined: Record<string, boolean> = {};

    public static alwaysFieldsForSelect: Record<string, boolean> = {
        ...super.alwaysFieldsForSelect,

        sellerAddress: true,
        policyID_CS: true,
        sellingToken_CS: true,
        sellingToken_TN: true,
        priceOfAsset: true,
        minADA: true,
    };

    // #endregion db
}
