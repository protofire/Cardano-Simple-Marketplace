import { initBackEnd } from '@example/src/lib/SmartDB/backEnd';
import { smartDBMainApiHandler } from 'smart-db/backEnd';

// This file is essential for initializing the backend of the application.
// It ensures that all necessary backend configurations, decorators, and handlers
// from the SmartDB library are properly set up for the application to function.

initBackEnd();
// Initializes the backend environment for the application by invoking the `initBackEnd` function.
// This function sets up registries, decorators, and any backend-specific configurations.

export const config = {
    api: {
        bodyParser: false,
    },
};
// Configures the Next.js API handler to disable the default body parser.
// This is particularly important if the API handler processes requests that include large payloads
// or uses a custom parsing mechanism like SmartDB.

export default smartDBMainApiHandler.bind(smartDBMainApiHandler);
// This is the main API handler for the backend, provided by the SmartDB library.
// It acts as an all-in-one function to manage requests, routing them to the appropriate endpoints.
// By binding the handler to itself, the method's internal context is preserved, ensuring proper execution.

