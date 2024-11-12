import { StoreProvider } from 'easy-peasy';
import { Session } from 'next-auth';
import { SessionProvider } from 'next-auth/react';
import type { AppProps } from 'next/app';
import { ReactNotifications } from 'react-notifications-component';
import 'react-notifications-component/dist/theme.css';
import { AppGeneral, CS, globalStore } from 'smart-db';
import Layout from '../components/UI/Layout/Layout';
import 'smart-db/dist/styles.css';
import { Address, MintingPolicy, SpendingValidator } from 'lucid-cardano';
import { createContext, Dispatch, SetStateAction, useEffect, useState } from 'react';
import 'styles/global.scss';
import { marketScript, mintingPolicyIDPreScript } from '../lib/Commons/Constants/onchain';

export type MenuClass = 'Buy' | 'Sell';

export type AppState = {
    // Global
    marketScript: SpendingValidator;
    marketAddress?: Address;
    mintingPolicyIDPreScript: MintingPolicy;
    mintingPolicyIDScript?: MintingPolicy;
    mintingPolicyID_CS?: CS;
    menuClass?: string;
};

const initialAppState: AppState = {
    marketScript: marketScript,
    mintingPolicyIDPreScript: mintingPolicyIDPreScript,
    menuClass: 'Buy',
};

export const AppStateContext = createContext<{
    appState: AppState;
    setAppState: Dispatch<SetStateAction<AppState>>;
}>({ appState: initialAppState, setAppState: () => {} });

export default function MyApp({ Component, pageProps }: AppProps<{ session?: Session }>) {
    const [appState, setAppState] = useState<AppState>(initialAppState);
    return (
        <AppStateContext.Provider value={{ appState, setAppState }}>
            <SessionProvider session={pageProps.session} refetchInterval={0}>
                <StoreProvider store={globalStore}>
                    <AppGeneral />
                    <ReactNotifications />
                    <Layout>
                        <Component {...pageProps} />
                    </Layout>
                </StoreProvider>
            </SessionProvider>
        </AppStateContext.Provider>
    );
}
