
import { Column, Entity, Index, PrimaryGeneratedColumn } from 'typeorm';
import { MarketNFTEntity } from './MarketNFT.Entity';
import { PostgreSQLAppliedFor, getPostgreSQLTableName } from 'smart-db';
import { BaseSmartDBEntityPostgreSQL, type CS, type TN } from 'smart-db/backEnd';
import { type PaymentKeyHash } from 'lucid-cardano';

// The class is decorated with @PostgreSQLAppliedFor, linking it to the MarketNFTEntity class for PostgreSQL-related operations.
@PostgreSQLAppliedFor([MarketNFTEntity])
// The @Entity decorator specifies the PostgreSQL table name based on the MarketNFTEntity class.
@Entity({ name: getPostgreSQLTableName(MarketNFTEntity.className()) })
// The @Index decorator creates an index on the sellerPaymentPKH field for faster lookups.
@Index(['sellerPaymentPKH']) 
export class MarketNFTEntityPostgreSQL extends BaseSmartDBEntityPostgreSQL {
    // The associated Entity for this PostgreSQL class, which is the MarketNFTEntity.
    protected static Entity = MarketNFTEntity;

    // #region internal class methods

    // Returns the static reference to this class (MarketNFTEntityPostgreSQL).
    public getPostgreSQLStatic(): typeof MarketNFTEntityPostgreSQL {
        return this.constructor as typeof MarketNFTEntityPostgreSQL;
    }

    // Static method that returns the static reference to this class (MarketNFTEntityPostgreSQL).
    public static getPostgreSQLStatic(): typeof MarketNFTEntityPostgreSQL {
        return this as typeof MarketNFTEntityPostgreSQL;
    }

    // Returns the static reference to the associated MarketNFTEntity class.
    public getStatic(): typeof MarketNFTEntity {
        return MarketNFTEntityPostgreSQL.getPostgreSQLStatic().getStatic() as typeof MarketNFTEntity;
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

    // #region fields

    // The auto-generated primary key for this entity.
    @PrimaryGeneratedColumn()
    _id!: number; 

    // The seller's payment public key hash.
    @Column({ type: "varchar", length: 255 })
    sellerPaymentPKH!: PaymentKeyHash;

    // The policy ID for the asset being sold.
    @Column({ type: "varchar", length: 255 })
    policyID_CS!: CS;

    // The selling token's CS.
    @Column({ type: "varchar", length: 255 })
    sellingToken_CS!: CS;

    // The token name of the selling token.
    @Column({ type: "varchar", length: 255 })
    sellingToken_TN!: TN;

    // The price of the asset being sold.
    @Column({ type: "varchar" })
    priceOfAsset!: string;

    // The minimum ADA required for the transaction.
    @Column({ type: "varchar" })
    minADA!: string;

    // Static method that returns the PostgreSQL model (reference to this class).
    public static PostgreSQLModel() {
        return this;
    }

    // #endregion fields
}

