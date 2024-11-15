import { initBackEnd } from '@example/src/lib/SmartDB/backEnd';
import { smartDBMainApiHandler } from 'smart-db/backEnd';

// TODO: agregar comentarios aqui, este archivo es fundamental para iniciar el back-end de la aplicacion

initBackEnd();


export const config = {
    api: {
        bodyParser: false,
    },
};

// TODO: explicar que esta es la funcion que se encarga de manejar las peticiones al back-end, todo all in one desde SmartDB
export default smartDBMainApiHandler.bind(smartDBMainApiHandler);
