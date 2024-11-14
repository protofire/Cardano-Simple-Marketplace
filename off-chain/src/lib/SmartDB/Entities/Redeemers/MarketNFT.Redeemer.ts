import { BaseTxRedeemer } from "smart-db";

// Define a type that represents a Redeemer for a Policy ID, 
// which can either be related to Minting or Burning an ID.
export type PolicyIdRedeemer = PolicyRedeemerMintID | PolicyRedeemerBurnID;

// Class for handling a Minting operation on a Policy ID.
// It extends from BaseTxRedeemer to inherit common functionality for a transaction redeemer.
export class PolicyRedeemerMintID extends BaseTxRedeemer {
    // The index of the Plutus data associated with this redeemer.
    protected static _plutusDataIndex = 1;
    
    // Indicates that this redeemer is a subtype of a more general type.
    protected static _plutusDataIsSubType = true;
}

// Class for handling a Burning operation on a Policy ID.
// It extends from BaseTxRedeemer to inherit common functionality for a transaction redeemer.
export class PolicyRedeemerBurnID extends BaseTxRedeemer {
    // The index of the Plutus data associated with this redeemer.
    protected static _plutusDataIndex = 2;
    
    // Indicates that this redeemer is a subtype of a more general type.
    protected static _plutusDataIsSubType = true;
}

// Define a type that represents a Redeemer for Market operations,
// which can either be related to a Buy or Withdraw operation.
export type MarketValidatorRedeemer = Buy | Withdraw;

// Class for handling a Buy operation in the Market.
// It extends from BaseTxRedeemer to inherit common functionality for a transaction redeemer.
export class Buy extends BaseTxRedeemer {
    // The index of the Plutus data associated with this redeemer.
    protected static _plutusDataIndex = 0;
    
    // Indicates that this redeemer is a subtype of a more general type.
    protected static _plutusDataIsSubType = true;
}

// Class for handling a Withdraw operation in the Market.
// It extends from BaseTxRedeemer to inherit common functionality for a transaction redeemer.
export class Withdraw extends BaseTxRedeemer {
    // The index of the Plutus data associated with this redeemer.
    protected static _plutusDataIndex = 1;
    
    // Indicates that this redeemer is a subtype of a more general type.
    protected static _plutusDataIsSubType = true;
}

