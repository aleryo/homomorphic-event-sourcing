import {combineReducers} from 'redux';

import {ChainName, GameState, Model, Player, PlayerType, SimpleMap} from './types';
import {Action} from './actions';

type Handler<Data> = (d: Data, a: Action) => Data;

interface Handlers<Data> {
    [key: string]: Handler<Data>;
}

function createReducer<Data>(initialState: Data, handlers: Handlers<Data>): Handler<Data> {
    return (state: Data = initialState, action: Action = {type: 'InitialAction'}) =>
        handlers.hasOwnProperty(action.type) ?
            handlers[action.type](state, action) :
            state;
}

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
        , displayMessages: true
        , errors: ['An error!', 'And another!']
        , domain: {host: '0', port: '0'}
        , wsServerUrl: ''
        , game: {type: 'Register', player: INITIAL_PLAYER}
    };


const strings = createReducer(INITIAL_STATE.strings, {});

const displayMessages = createReducer(INITIAL_STATE.displayMessages, {
    ['ShowMessages']: (data: boolean, action: Action) => true,
    ['HideMessages']: (data: boolean, action: Action) => false
});

const errors = createReducer(INITIAL_STATE.errors, {});

const domain = createReducer(INITIAL_STATE.domain, {});

const wsServerUrl = createReducer(INITIAL_STATE.wsServerUrl, {});

function isEmpty(str?: string) {
    return (!str || !str.trim());
}

const player = (name: string): Player => ({
    playerName: name,
    playerType: PlayerType.Human,
    tiles: [],
    ownedStock: new SimpleMap<ChainName, number>(),
    ownedCash: 6000
});

function game(game: GameState = INITIAL_STATE.game, action: Action = {type: 'InitialAction'}) {
    switch (game.type) {

        case 'Register':
            switch (action.type) {
                case 'SetName':
                    return {type: 'Register', player: player(action.name)};
                case 'RegisterPlayer':
                    if (isEmpty(game.player.playerName)) {
                        return game;
                    }
                    // TODO send to backend: sendCommand model List
                    return {type: 'SelectGame', player: game.player, games: [], numPlayers: 1, numRobots: 5};
            }
            return game;

        case 'SelectGame':
            switch (action.type) {
                case 'SetNumPlayers': {
                    const number: number = parseInt(action.num, 10);
                    if(isNaN(number)){
                        return game;
                    }
//                    return Object.assign({}, game, {numPlayers: number});
                    return { type: game.type, player: game.player, games: game.games, numPlayers: number, numRobots: game.numRobots };
                }
                case 'SetNumRobots': {
                    const number: number = parseInt(action.num, 10);
                    if(isNaN(number)){
                        return game;
                    }
                    return { type: game.type, player: game.player, games: game.games, numPlayers: game.numPlayers, numRobots: number };
                }
                case 'CreateGame':
                    // TODO send to backend: sendCommand model (NewGame { numHumans = sg.numPlayers, numRobots = sg.numRobots })
                    return game;
            }
            return game;

        case 'PlayGame':
            return game;

        case 'EndOfGame':
            return game;
    }
    return game;
}

export default combineReducers({
    strings,
    displayMessages,
    errors,
    domain,
    wsServerUrl,
    game
});
