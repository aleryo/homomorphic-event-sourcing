import {combineReducers} from 'redux';

import {ChainName, Game, GameState, Model, Player, PlayerType, SimpleMap, Stock} from './types';
import {Action} from './actions';

type Handler<Data> = (d:Data, a:Action) => Data;

interface Handlers<Data> {
    [key:string] : Handler<Data>;
}

function createReducer<Data>(initialState : Data, handlers : Handlers<Data>) : Handler<Data> {
    return (state : Data = initialState, action : Action = { type: 'InitialAction' }) =>
        handlers.hasOwnProperty(action.type) ?
            handlers[action.type](state, action) :
            state
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


const strings = createReducer(INITIAL_STATE.strings, {
});

const displayMessages = createReducer(INITIAL_STATE.displayMessages, {
    ['ShowMessages']: (data:boolean, action:Action) => true,
    ['HideMessages']: (data:boolean, action:Action) => false
});

const errors = createReducer(INITIAL_STATE.errors, {
});

const domain = createReducer(INITIAL_STATE.domain, {
});

const wsServerUrl = createReducer(INITIAL_STATE.wsServerUrl, {
});

function isEmpty(str?: string) {
    return (!str || !str.trim());
}

const player = (name: string) : Player => ({
    playerName: name,
    playerType: PlayerType.Human,
    tiles: [],
    ownedStock: new SimpleMap<ChainName,number>(),
    ownedCash: 6000
});

const game = createReducer(INITIAL_STATE.game, {
    ['SetName']: (game:GameState, action:Action) => {
        switch(game.type) {
            case 'Register':
                switch (action.type) { // FIXME: stupid hack to work around untyped action
                    case 'SetName':
                        return { type: 'Register', player: player(action.name) };
                }
        }
        return game;
    },
    ['RegisterPlayer']: (game:GameState, action:Action) => {
        switch (game.type){
            case 'Register':
                if (!isEmpty(game.player.playerName)) {
                    // TODO send to backend
                    return {type: 'SelectGame', player: game.player, games: [], numPlayers: 1, numRobots: 5};
                }
                break;
        }
        return game;
    }
});

export default combineReducers({
    strings,
    displayMessages,
    errors,
    domain,
    wsServerUrl,
    game
});
