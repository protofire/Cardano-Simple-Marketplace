import 'reflect-metadata';

import { Convertible, BaseSmartDBEntity, asSmartDBEntity } from 'smart-db';

import { type Assets, type PaymentKeyHash,  } from 'lucid-cardano';

@asSmartDBEntity()

export class MarketNFTEntity extends BaseSmartDBEntity {
    protected static _apiRoute: string = 'marketnft';
    protected static _className: string = 'MarketNFT';

    // #region fields

    @Convertible( { isForDatum: true,  } )
    sellerAddress!:  PaymentKeyHash ;
    
    @Convertible({ isForDatum: true,  })
    policyID!: String;
   
    @Convertible({ isForDatum: true,  })
    sellingToken_CS!: String;

    @Convertible({ isForDatum: true,  })
    sellingToken_TN!: String;
    
    @Convertible( { isForDatum: true,  } )
    priceOfAsset!: BigInt;

    // #endregion fields

    // #region db

    public static defaultFieldsWhenUndefined: Record<string, boolean> = {};

    public static alwaysFieldsForSelect: Record<string, boolean> = {
        ...super.alwaysFieldsForSelect,
          policyID: true,
          sellerAddress: true,
          sellingToken: true,
          priceOfAsset: true,
    };

    // #endregion db
}
