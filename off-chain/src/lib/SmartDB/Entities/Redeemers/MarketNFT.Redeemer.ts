import { BaseTxRedeemer } from "smart-db";


export type PolicyIdRedeemer = PolicyRedeemerMintID | PolicyRedeemerBurnID;

export class PolicyRedeemerMintID extends BaseTxRedeemer {
    protected static _plutusDataIndex = 1;
    protected static _plutusDataIsSubType = true;
}

export class PolicyRedeemerBurnID extends BaseTxRedeemer {
    protected static _plutusDataIndex = 2;
    protected static _plutusDataIsSubType = true;
}

export type  MarketValidatorRedeemer = Buy | Withdraw;

export class Buy extends BaseTxRedeemer {
    protected static _plutusDataIndex = 0;
    protected static _plutusDataIsSubType = true;
}

export class Withdraw extends BaseTxRedeemer {
    protected static _plutusDataIndex = 1;
    protected static _plutusDataIsSubType = true;
}

