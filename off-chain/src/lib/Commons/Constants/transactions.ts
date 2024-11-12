import { Script } from 'lucid-cardano';
import { scriptSchema, yup } from 'smart-db/backEnd';

export const MARKET_SELL = 'MarketNFT - Sell';
export const MARKET_BUY = 'MarketNFT - Buy';
export const MARKET_WITHDRAW = 'MarketNFT - Withdraw';

export interface SellMarketNFTTxParams {
    token_TN: string;
    token_CS: string;
    datumID_CS: string;
    datumID_TN: string;
    validatorAddress: string;
    mintingPolicyID: Script;
    validatorMarket: Script;
    priceOfAsset: bigint;
}
export interface BuyMarketNFTTxParams {
    token_TN: string;
    token_CS: string;
    datumID_CS: string;
    datumID_TN: string;
    marketNft_id: string;
    sellerAddress: string;
    mintingPolicyID: Script;
    validatorMarket: Script;
    priceOfAsset: bigint;
}

export interface WithdrawMarketNFTTxParams {
    token_TN: string;
    token_CS: string;
    datumID_CS: string;
    datumID_TN: string;
    marketNft_id: string;
    mintingPolicyID: Script;
    validatorMarket: Script;
}
export const sellMarketNFTTxParamsSchema = yup.object().shape({
    token_TN: yup.string().required(),
    token_CS: yup.string().required(),
    datumID_CS: yup.string().required(),
    datumID_TN: yup.string().required(),
    validatorAddress: yup.string().required(),
    mintingPolicyID: scriptSchema.required(),
    validatorMarket: scriptSchema.required(),
    priceOfAsset: yup.number().required(),
});

export const buyMarketNFTTxParamsSchema = yup.object().shape({
    token_TN: yup.string().required(),
    token_CS: yup.string().required(),
    datumID_CS: yup.string().required(),
    datumID_TN: yup.string().required(),
    marketNft_id:  yup.string().required(),
    sellerPaymentPKH:  yup.string().required(),
    mintingPolicyID: scriptSchema.required(),
    validatorMarket: scriptSchema.required(),
    priceOfAsset: yup.number().required(),
});

export const withdrawMarketNFTTxParamsSchema = yup.object().shape({
    token_TN: yup.string().required(),
    token_CS: yup.string().required(),
    datumID_CS: yup.string().required(),
    datumID_TN: yup.string().required(),
    marketNft_id:  yup.string().required(),
    mintingPolicyID: scriptSchema.required(),
    validatorMarket: scriptSchema.required(),
});
