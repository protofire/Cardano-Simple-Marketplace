import { Column, Entity, Index, PrimaryGeneratedColumn } from 'typeorm';
import { MarketNFTEntity } from './MarketNFT.Entity';
import { PostgreSQLAppliedFor, getPostgreSQLTableName } from 'smart-db';
import {  BaseSmartDBEntityPostgreSQL } from 'smart-db/backEnd';
import { type PaymentKeyHash,  } from 'lucid-cardano';

@PostgreSQLAppliedFor([MarketNFTEntity])
@Entity({ name: getPostgreSQLTableName(MarketNFTEntity.className()) })
@Index(['ddPaymentPKH', ]) // Add indices as needed
export class MarketNFTEntityPostgreSQL extends  BaseSmartDBEntityPostgreSQL {
    protected static Entity = MarketNFTEntity;

    // #region internal class methods

    public getPostgreSQLStatic(): typeof MarketNFTEntityPostgreSQL {
        return this.constructor as typeof MarketNFTEntityPostgreSQL;
    }

    public static getPostgreSQLStatic(): typeof MarketNFTEntityPostgreSQL {
        return this as typeof MarketNFTEntityPostgreSQL;
    }

    public getStatic(): typeof MarketNFTEntity {
        return MarketNFTEntityPostgreSQL.getPostgreSQLStatic().getStatic() as typeof MarketNFTEntity;
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

    // #region fields

    @PrimaryGeneratedColumn()
    _id!: number; // Auto-generated primary key

    @Column({ type: "varchar", length: 255  })
    policyID_TN!:string;
    @Column({ type: "varchar", length: 255  })
    sellerAddress!: PaymentKeyHash ;
    @Column({ type: "varchar", length: 255  })
    sellingToken_TN!:string;
    @Column({ type: "int"  })
    priceOfAsset!:number;

    public static PostgreSQLModel() {
        return this;
    }
    // #endregion fields
}
