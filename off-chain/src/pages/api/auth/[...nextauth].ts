import { initBackEnd } from '@example/src/lib/SmartDB/backEnd';
import { smartDBMainApiHandler } from 'smart-db/backEnd';

//TODO: volver a comentar este llamado, todos los archivos de backend deben llamarlo si o si al iniciar

initBackEnd();

export const config = {
    api: {
        bodyParser: false,
    },
};

export default smartDBMainApiHandler.bind(smartDBMainApiHandler);
