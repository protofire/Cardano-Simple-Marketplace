import 'reflect-metadata';

import { Convertible, BaseSmartDBEntity, asSmartDBEntity } from 'smart-db';

import { type PaymentKeyHash,  } from 'lucid-cardano';

@asSmartDBEntity()

export class MarketNFTEntity extends BaseSmartDBEntity {
    protected static _apiRoute: string = 'marketnft';
    protected static _className: string = 'MarketNFT';

    // #region fields

    @Convertible()
    policyID_TN!: string;
    @Convertible( { isForDatum: true,  } )
    sellerAddress!:  PaymentKeyHash ;
    @Convertible()
    sellingToken_TN!: string;
    @Convertible( { isForDatum: true,  } )
    priceOfAsset!: number;

    // #endregion fields

    // #region db

    public static defaultFieldsWhenUndefined: Record<string, boolean> = {};

    public static alwaysFieldsForSelect: Record<string, boolean> = {
        ...super.alwaysFieldsForSelect,
          policyID_TN: true,
          sellerAddress: true,
          sellingToken_TN: true,
          priceOfAsset: true,
    };

    // #endregion db
}
