## Table of Contents
- [Table of Contents](#table-of-contents)
- [Usage](#usage)
- [Project Code Structure](#project-code-structure)
  - [Directories](#directories)
  - [Configuration and Contracts](#configuration-and-contracts)
  - [Components and Pages](#components-and-pages)
  - [Library Implementation and Entities files](#library-implementation-and-entities-files)
  - [API and Backend](#api-and-backend)
- [Swagger Server and UI](#swagger-server-and-ui)
  - [Run Swagger Server](#run-swagger-server)
  - [Access Swagger UI](#access-swagger-ui)
  - [Use Health Endpoint](#use-health-endpoint)
  - [Download Swagger JSON](#download-swagger-json)
- [Using Postman](#using-postman)
  - [Import JSON](#import-json)
  - [Test API in Postman: Health Endpoint](#test-api-in-postman-health-endpoint)
  - [Set Bearer Token in Postman](#set-bearer-token-in-postman)

  
## Usage

The Cardano Simple Marketplace web application provides a seamless user experience for interacting with the marketplace and managing tokens. Below are the key functionalities and their usage:

### **Connect Wallet**:
   - Upon visiting the homepage at `http://localhost:3000/`, users are prompted to connect their preferred Cardano wallet.
   - The wallet connection is essential for interacting with the marketplace and conducting transactions.
   - Supported wallets allow users to connect and authorize actions such as buying and selling tokens.

### **Browse and View Tokens**:
   - Users can browse all tokens listed for sale in the marketplace.
   - Each token listing displays relevant details such as:
     - Token name
     - Seller information
     - Price in ADA
     - Token metadata (e.g., images, descriptions)
   - Tokens that are available for purchase are displayed with "Buy" buttons, while tokens that are being sold by the user (if logged in) are shown with "Withdraw" options.

### **Buy Tokens**:
   - Users can select a token to purchase by clicking the **Buy** button next to the listed token.
   - The token price is shown in ADA, and the purchase transaction is initiated upon confirmation.
   - After initiating the buy action, users will be prompted to confirm the transaction through their connected wallet.

### **Sell Tokens**:
   - Users can list their own tokens for sale by clicking the **Sell** button.
   - To list a token, users must enter the amount they wish to sell and set a price in ADA.
   - Once the listing is confirmed, the token will appear in the marketplace as available for purchase.

### **Withdraw Tokens from Sale**:
   - Users who have previously listed tokens for sale can withdraw them from the marketplace.
   - By clicking the **Withdraw** button next to their listed token, users can remove their token from the marketplace.
   - This action is only available to the token creator (seller) and is enforced by the smart contract.

### **Transaction Feedback**:
   - After initiating a buy, sell, or withdraw action, users will see real-time feedback through modals indicating the status of the transaction.
   - These modals provide information such as:
     - Transaction hash
     - Whether the transaction was successful or failed
     - Confirmation of the transaction once it has been processed on the blockchain

All of these actions require a connected wallet and are securely processed using Cardano's blockchain technology.
## Project Code Structure

The example code is organized to showcase the usage of the Smart DB library, providing a clear structure for ease of understanding and modification.

### Directories

```
 .
├──  _config
│   └──  protocol-parameters.json
├──  _smart-contracts
│   ├──  policyID.plutus
│   └──  validator.plutus
├──  _swagger
│   ├──  server.ts
│   └──  swagger.ts
├──  public
│   ├──  img
│   │   └──  wallet
│   │       ├──  eternl.png
│   │       ├──  flint.png
│   │       ├──  nami.png
│   │       ├──  nufi.png
│   │       ├──  typhon.png
│   │       └──  yoroi.png
│   ├──  favicon.ico
│   ├──  next.svg
│   ├──  swagger.json
│   └──  vercel.svg
├── 󱧼 src
│   ├──  components
│   │   ├──  Commons
│   │   │   ├──  LoaderButton
│   │   │   │   ├──  LoaderButton.module.scss
│   │   │   │   └──  LoaderButton.tsx
│   │   │   ├──  ModalTransaction
│   │   │   │   ├──  ModalTransaction.module.scss
│   │   │   │   ├──  ModalTransaction.tsx
│   │   │   │   └──  useModalTransaction.tsx
│   │   │   ├──  ModalUTxOs
│   │   │   │   ├──  DetailsBalance
│   │   │   │   │   ├──  DetailsBalance.module.scss
│   │   │   │   │   ├──  DetailsBalance.tsx
│   │   │   │   │   └──  useDetailsBalance.tsx
│   │   │   │   ├──  DetailsUTxO
│   │   │   │   │   ├──  DetailsUTxO.module.scss
│   │   │   │   │   ├──  DetailsUTxO.tsx
│   │   │   │   │   └──  useDetailsUTxO.tsx
│   │   │   │   ├──  ModalUTxOs.module.scss
│   │   │   │   ├──  ModalUTxOs.tsx
│   │   │   │   └──  useModalUTxOs.tsx
│   │   │   ├──  TokenCard
│   │   │   │   ├──  TokenBuy
│   │   │   │   │   ├──  TokenBuy.module.scss
│   │   │   │   │   ├──  TokenBuy.tsx
│   │   │   │   │   └──  useTokenBuy.ts
│   │   │   │   ├──  TokenItem
│   │   │   │   │   ├──  TokenItem.module.scss
│   │   │   │   │   └──  TokenItem.tsx
│   │   │   │   ├──  TokenSell
│   │   │   │   │   ├──  TokenSell.module.scss
│   │   │   │   │   ├──  TokenSell.tsx
│   │   │   │   │   └──  useTokenSell.ts
│   │   │   │   ├──  TokenCard.module.scss
│   │   │   │   └──  TokenCard.tsx
│   │   │   └──  WalletConnector
│   │   │       ├──  WalletInfo
│   │   │       │   ├──  WalletApiKey
│   │   │       │   │   ├──  useWalletApiKey.tsx
│   │   │       │   │   ├──  WalletApiKey.module.scss
│   │   │       │   │   └──  WalletApiKey.tsx
│   │   │       │   ├──  WalletInfo.module.scss
│   │   │       │   └──  WalletInfo.tsx
│   │   │       ├──  WalletList
│   │   │       │   ├──  WalletList.module.scss
│   │   │       │   └──  WalletList.tsx
│   │   │       ├──  WalletConnector.module.scss
│   │   │       └──  WalletConnector.tsx
│   │   ├──  public
│   │   │   ├──  Buy
│   │   │   │   ├──  Buy.module.scss
│   │   │   │   ├──  Buy.tsx
│   │   │   │   └──  useBuy.ts
│   │   │   ├──  Home
│   │   │   │   ├──  Home.module.scss
│   │   │   │   ├──  Home.tsx
│   │   │   │   └──  useHome.ts
│   │   │   └──  Sell
│   │   │       ├──  Sell.module.scss
│   │   │       ├──  Sell.tsx
│   │   │       └──  useSell.ts
│   │   └──  UI
│   │       └──  Layout
│   │           ├──  Layout.module.scss
│   │           └──  Layout.tsx
│   ├──  lib
│   │   ├──  Commons
│   │   │   └──  Constants
│   │   │       ├──  onchain.ts
│   │   │       └──  transactions.ts
│   │   └──  SmartDB
│   │       ├──  BackEnd
│   │       │   ├──  index.ts
│   │       │   └──  MarketNFT.BackEnd.Api.Handlers.ts
│   │       ├──  Entities
│   │       │   ├──  Redeemers
│   │       │   │   └──  MarketNFT.Redeemer.ts
│   │       │   ├──  index.BackEnd.ts
│   │       │   ├──  index.ts
│   │       │   ├──  MarketNFT.Entity.Mongo.ts
│   │       │   ├──  MarketNFT.Entity.PostgreSQL.ts
│   │       │   └──  MarketNFT.Entity.ts
│   │       ├──  FrontEnd
│   │       │   ├──  index.ts
│   │       │   └──  MarketNFT.FrontEnd.Api.Calls.ts
│   │       └──  backEnd.ts
│   └──  pages
│       ├── 󰒍 api
│       │   ├──  auth
│       │   │   └──  [...nextauth].ts
│       │   └──  [[...query]].ts
│       ├──  _app.tsx
│       ├──  _document.tsx
│       ├──  index.module.scss
│       └──  index.tsx
└──  styles
    ├──  _animations.scss
    ├──  _mixins.scss
    ├──  global.scss
    └──  shared.module.scss
```

### Configuration and Contracts
- `_config`: Contains configuration files for various tools within the project.
- `_smart-contracts`: Stores the Plutus files with the cbor hex code of the scripts used in the Marketplace. While the code is hardcoded in the Home component, it's maintained here for reference.

### Components and Pages
- `src/components`: React components used throughout the application reside here.
- `src/pages`: New pages can be added within this directory. Essential files like `_app.tsx`, `_document.tsx`, and `index.tsx` are set up to load the `Home` component by default.

### Library Implementation and Entities files
- `src/lib/SmartDB`: Sample implementation directory for creating entities that are synced with the blockchain. This directory should contain all entity models required by a project, whether they are synced with the blockchain (like the MarketNFT Entity) or simply database-backed entities with full API support for the frontend and backend.

Read this section for further clarifications:
[Setting Up New Projects](#setting-up-new-projects)

### API and Backend
- `src/pages/api`: Defines the API routes and backend logic for server-side operations.

## Swagger Server and UI

The example project includes a Swagger server to provide a user-friendly interface for testing API endpoints.

### Run Swagger Server

```
npm run swagger-start
```

### Access Swagger UI

Visit `http://localhost:3001/docs/` in your browser to access the Swagger UI.

### Use Health Endpoint

- To test the health endpoint, click on the `/health` endpoint in the Swagger UI.
- Click the "Try it out" button, then click "Execute" to see the response.

### Download Swagger JSON

To download the Swagger JSON file:

- Visit `http://localhost:3000/swagger.json`.
- Save the JSON file to your local machine.

## Using Postman

You can import the Swagger JSON into Postman to test the API endpoints.

### Import JSON

1. Open Postman and click "Import".
2. Select the "Link" tab.
3. Enter the URL to your saved Swagger JSON file or upload the file directly.
4. Click "Import".

### Test API in Postman: Health Endpoint

1. In Postman, find the imported collection and select the `/health` endpoint.
2. Click "Send" to test the endpoint.

### Set Bearer Token in Postman

To use the API key generated in the wallet connected modal:

1. In Postman, go to the "Authorization" tab for the endpoint you want to test.
2. Select "Bearer Token" from the "Type" dropdown.
3. Enter the API key in the "Token" field.
4. Click "Send" to test the endpoint with the Bearer Token.