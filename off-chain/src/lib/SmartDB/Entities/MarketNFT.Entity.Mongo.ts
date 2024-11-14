import { model, models, Schema } from 'mongoose';
import 'reflect-metadata';
import { MongoAppliedFor } from 'smart-db';
import { BaseSmartDBEntityMongo, CS, IBaseSmartDBEntity, TN } from 'smart-db/backEnd';
import { MarketNFTEntity } from './MarketNFT.Entity';
import { type PaymentKeyHash } from 'lucid-cardano';

// The class is decorated with @MongoAppliedFor, indicating that it's used for MongoDB-related operations
// and is applied to the MarketNFTEntity class.
@MongoAppliedFor([MarketNFTEntity])
export class MarketNFTEntityMongo extends BaseSmartDBEntityMongo {
    // The associated Entity for this Mongo class, which is the MarketNFTEntity.
    protected static Entity = MarketNFTEntity;
    
    // The MongoDB table name corresponding to this entity.
    protected static _mongoTableName: string = MarketNFTEntity.className();

    // #region fields
    // Here you would define the fields for this MongoDB model,
    // although they are commented out for now. They would include:
    // policyID_TN: String, sellerPaymentPKH: PaymentKeyHash, sellingToken_TN: String, priceOfAsset: Int
    // #endregion fields

    // #region internal class methods

    // Returns the static reference to this class (MarketNFTEntityMongo).
    public getMongoStatic(): typeof MarketNFTEntityMongo {
        return this.constructor as typeof MarketNFTEntityMongo;
    }

    // Static method that returns the static reference to this class (MarketNFTEntityMongo).
    public static getMongoStatic(): typeof MarketNFTEntityMongo {
        return this as typeof MarketNFTEntityMongo;
    }

    // Returns the static reference to the associated MarketNFTEntity class.
    public getStatic(): typeof MarketNFTEntity {
        return this.getMongoStatic().getStatic() as typeof MarketNFTEntity;
    }

    // Static method that returns the static reference to the associated MarketNFTEntity class.
    public static getStatic(): typeof MarketNFTEntity {
        return this.Entity as typeof MarketNFTEntity;
    }

    // Method to get the class name of the associated entity (MarketNFTEntity).
    public className(): string {
        return this.getStatic().className();
    }

    // Static method to get the class name of the associated entity (MarketNFTEntity).
    public static className(): string {
        return this.getStatic().className();
    }

    // #endregion internal class methods

    // #region mongo db

    // Static method to create and return the MongoDB model for this entity.
    public static MongoModel() {
        // Interface that defines the structure of the MongoDB document for this entity.
        interface Interface {
            sellerPaymentPKH: PaymentKeyHash;
            policyID_CS: CS;
            sellingToken_CS: CS;
            sellingToken_TN: TN;
            priceOfAsset: BigInt;
            minADA: BigInt;
        }

        // Schema definition for the MongoDB collection, specifying the types and requirements for each field.
        const schema = new Schema<Interface>({
            sellerPaymentPKH: { type: String, required: true },
            policyID_CS: { type: String, required: true },
            sellingToken_CS: { type: String, required: true },
            sellingToken_TN: { type: String, required: true },
            priceOfAsset: { type: Number, required: true },
            minADA: { type: Number, required: true },
        });

        // If the model already exists in the database, use it; otherwise, create a new model.
        const ModelDB = models[this._mongoTableName] || model<Interface>(this._mongoTableName, schema);
        
        // Return the MongoDB model for this entity.
        return ModelDB;
    }

    // #endregion mongo db
}

