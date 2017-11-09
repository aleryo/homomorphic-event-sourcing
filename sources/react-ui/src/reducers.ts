
import {ChainName, Model, PlayerType, SimpleMap, Stock} from './types';

const INITIAL_PLAYER = {
    playerName: 'Fred',
    playerType: PlayerType.Human,
    tiles: [],
    ownedStock: new SimpleMap<ChainName, number>(),
    ownedCash: 0
};

export const INITIAL_STATE: Model =
    {
        strings: ['A string!', 'And another!']
        , showMessages: true
        , errors: ['An error!', 'And another!']
        , domain: {host: '0', port: '0'}
        , wsServerUrl: ''
        , game: {gameType: 'Register', player: INITIAL_PLAYER}
    };
