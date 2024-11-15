import type { NextPage } from 'next';
import styles from './index.module.scss';
import Home from '../components/public/Home/Home';
//TODO: comentarios en este archivo

const HomePage: NextPage = () => {
    return (
        <>
            <main >
                <Home />
            </main>
        </>
    );
};

export default HomePage;
