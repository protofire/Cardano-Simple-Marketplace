import { model, models, Schema } from 'mongoose';
import 'reflect-metadata';
import { MongoAppliedFor } from 'smart-db';
import { BaseSmartDBEntityMongo, CS, IBaseSmartDBEntity, TN } from 'smart-db/backEnd';
import { MarketNFTEntity } from './MarketNFT.Entity';
import { type PaymentKeyHash } from 'lucid-cardano';

@MongoAppliedFor([MarketNFTEntity])
export class MarketNFTEntityMongo extends BaseSmartDBEntityMongo {
    protected static Entity = MarketNFTEntity;
    protected static _mongoTableName: string = MarketNFTEntity.className();

    // #region fields

    // policyID_TN:String
    // sellerAddress: PaymentKeyHash
    // sellingToken_TN:String
    // priceOfAsset:Int

    // #endregion fields

    // #region internal class methods

    public getMongoStatic(): typeof MarketNFTEntityMongo {
        return this.constructor as typeof MarketNFTEntityMongo;
    }

    public static getMongoStatic(): typeof MarketNFTEntityMongo {
        return this as typeof MarketNFTEntityMongo;
    }

    public getStatic(): typeof MarketNFTEntity {
        return this.getMongoStatic().getStatic() as typeof MarketNFTEntity;
    }

    public static getStatic(): typeof MarketNFTEntity {
        return this.Entity as typeof MarketNFTEntity;
    }

    public className(): string {
        return this.getStatic().className();
    }

    public static className(): string {
        return this.getStatic().className();
    }

    // #endregion internal class methods

    // #region mongo db

    public static MongoModel() {
        interface Interface {
            sellerAddress: PaymentKeyHash;
            policyID_CS: CS;
            sellingToken_CS: CS;
            sellingToken_TN: TN;
            priceOfAsset: BigInt;
            minADA: BigInt;
        }

        const schema = new Schema<Interface>({
            sellerAddress: { type: String, required: true },
            policyID_CS: { type: String, required: true },
            sellingToken_CS: { type: String, required: true },
            sellingToken_TN: { type: String, required: true },
            priceOfAsset: { type: Number, required: true },
            minADA: { type: Number, required: true },
        });

        const ModelDB = models[this._mongoTableName] || model<Interface>(this._mongoTableName, schema);
        return ModelDB;
    }

    // #endregion mongo db
}
