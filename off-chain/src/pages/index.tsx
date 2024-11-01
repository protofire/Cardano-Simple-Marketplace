import type { NextPage } from 'next';
import styles from './index.module.scss';
import Home from '../components/public/Home/Home';

const HomePage: NextPage = () => {
    return (
        <>
            <body>
                <Home />
            </body>
        </>
    );
};

export default HomePage;
