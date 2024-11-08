
import { Schema, model, models } from 'mongoose';
import 'reflect-metadata';
import { MongoAppliedFor  } from 'smart-db';
import {  BaseSmartDBEntityMongo, IBaseSmartDBEntity } from 'smart-db/backEnd';
import { MarketNFTEntity } from './MarketNFT.Entity';
import {type Assets, type PaymentKeyHash,  } from 'lucid-cardano';

@MongoAppliedFor([MarketNFTEntity])
export class MarketNFTEntityMongo extends  BaseSmartDBEntityMongo {
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
            policyID: Assets;
            sellerAddress:  PaymentKeyHash ;
            sellingToken: Assets;
            priceOfAsset: number;
        }

        const schema = new Schema<Interface>({
            policyID: { type: String, required: true },
            sellerAddress: { type: String, required: true },
            sellingToken: { type: String, required: true },
            priceOfAsset: { type: Number, required: true },
        });

        const ModelDB = models[this._mongoTableName] || model<Interface>(this._mongoTableName, schema);
        return ModelDB;
    }

    // #endregion mongo db
}

