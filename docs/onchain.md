# Smart Contracts

This document outlines the on-chain structure and scripts used in the Cardano Simple Marketplace project, detailing the Smart Contracts and the deployment process.
#Table of Contents

- [Project Structure](#project-structure)
   - [Market Folder](#market-folder)
     - [Policy ID](#policy-id)
     - [Policy NFT](#nft-policy)
     - [MarketNFT Validator](#marketnft-validator)
   - [Scripts Folder](#scripts-folder)
     - [cli.sh](#scripts-cli.sh)
- [Key Functions of `cli.sh`](#key-functions-of-clish)
   - [Deploy Policies](#deploy-policies)
   - [Deploy Validator](#deploy-validator)
   - [Testing](#testing)
- [Conclusion](#conclusion)
## Project Structure

### Market Folder
The **`Market`** folder contains two critical files: two policies and a validator, which form the core of the on-chain logic for the marketplace.

1. **Policy ID** (`Market/PolicyID.hs`):
   - This policy governs the creation and control of unique identifiers for each token in the marketplace.
   - It ensures that each token has a unique identifier, providing a mechanism for verifying the authenticity and ownership of tokens.

2. **Policy NFT** (`Market/PolicyNFT.hs`):
   - This policy is responsible for managing the minting and transferring of NFT tokens within the marketplace.
   - It enforces rules for creating tokens that represent digital assets and ensures they adhere to the marketplace's token standards.
   - This policy is tied to the **MarketNFT** validator and ensures that only valid transactions can be executed for these tokens.

3. **MarketNFT Validator** (`Market/MarketNFTValidator.hs`):
   - The **MarketNFT** validator is responsible for processing all buy, sell, and withdrawal actions for tokens listed on the marketplace.
   - It enforces the business rules around transactions, ensuring that only the rightful owner can sell or withdraw tokens and that all token transfers comply with the marketplace’s rules.
   - The validator listens to transactions related to NFT tokens, verifies their authenticity, and performs actions like buying or selling tokens.

### Scripts Folder
In the **`scripts`** folder, the **`cli.sh`** script is included, allowing developers to interact with the on-chain smart contracts easily. This script simplifies the testing and deployment of the contracts and policies. 

- **`scripts/cli.sh`**:
  - This script is used to deploy the **MarketNFT Validator**, as well as the **ID Policy** and **NFT Policy**.
  - It provides a convenient way to test the functionality of the contracts without requiring any parameters. 
  - The script automates the deployment process and ensures that the smart contracts are properly configured on the Cardano blockchain.

### Key Functions of `cli.sh`:
- **Deploy Policies**: Deploys the **ID Policy** and **NFT Policy** to the blockchain. These policies are used for minting and managing NFT tokens in the marketplace.
- **Deploy Validator**: Deploys the **MarketNFT Validator**, enabling the marketplace's buy, sell, and withdraw functionality.
- **Testing**: The script includes commands to test the deployed contracts, ensuring that the marketplace functions as expected with the on-chain logic.

This on-chain setup ensures that the Cardano Simple Marketplace can function seamlessly, managing token listings, purchases, sales, and withdrawals in a secure, decentralized environment.