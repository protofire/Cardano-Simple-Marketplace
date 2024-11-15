import { EndpointsManager, initBackEnd as initBackEndSmartDB } from 'smart-db/backEnd';
// Importing the SmartDB backend initialization function and the EndpointsManager to manage API endpoints.

export * from 'smart-db/backEnd';
// Re-exporting all modules from 'smart-db/backEnd' to make them available to other parts of the application.

export * from './BackEnd/index';
// Re-exporting all backend-related modules from a local directory to centralize imports.

export * from './Entities/index.BackEnd';
// Re-exporting all entity-related backend modules from a local directory to centralize imports.

/* 
NOTE: This file acts as the Root Backend File and is crucial for backend configuration.
It aggregates imports from all backend-related code (endpoints, entities, handlers, etc.)
to ensure that all decorators and registries are correctly initialized.
This ensures proper functioning of the backend environment and the SmartDB library. 
*/

export function initBackEnd() {
    // Initializing the backend environment for SmartDB.
    initBackEndSmartDB();

    // Creating a singleton instance of the EndpointsManager, responsible for managing API endpoint access levels.
    const endpointsManager = EndpointsManager.getInstance();

    // Setting public endpoints using regular expressions.
    // These endpoints will be accessible without requiring authentication, unlike the default secured behavior.
    // Example of exposing a set of public endpoints:
    // endpointsManager.setPublicEndPointsInternet([/^\/api\/free\/?.*/]);

    endpointsManager.setPublicEndPointsInternet([/^\/api\/blockfrost\/.+/]);
    // Allowing public access to endpoints matching the `/api/blockfrost/` pattern.

    endpointsManager.setPublicEndPointsInternet([/^\/api\/transactions\/.+/]);
    // Allowing public access to endpoints matching the `/api/transactions/` pattern.
}

/* 
Summary:
1. This code configures the backend for projects using the SmartDB library.
2. It ensures proper loading of all decorators and backend registries.
3. It provides functionality to manage API endpoints, enabling public access for specified routes.
4. Developers can modify the `setPublicEndPointsInternet` calls to define which API routes should be publicly accessible.
*/

