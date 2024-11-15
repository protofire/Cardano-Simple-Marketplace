
import { initBackEnd } from '@example/src/lib/SmartDB/backEnd';
import { smartDBMainApiHandler } from 'smart-db/backEnd';

// This function call must be included in every backend file, as it initializes the backend environment.
// It loads all necessary configurations, decorators, and registries for entities and backend handlers.
// Calling `initBackEnd()` ensures the backend is correctly set up and ready to handle requests.
initBackEnd();

export const config = {
    api: {
        // The `bodyParser: false` setting disables the default body parser for API requests.
        // This may be necessary when working with custom request formats or file uploads.
        bodyParser: false,
    },
};

// The `smartDBMainApiHandler` handles all API requests for the backend.
// It acts as the main handler for incoming requests, routing them according to the defined endpoints,
// and processing them using the SmartDB framework.
export default smartDBMainApiHandler.bind(smartDBMainApiHandler);

