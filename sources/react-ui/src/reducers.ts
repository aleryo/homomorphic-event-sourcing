import {combineReducers} from 'redux';

import {Cell, ChainName, GameState, Messages, Model, Player, PlayerType, SimpleMap, Tile} from './types';
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
    ownedStock: new SimpleMap<ChainName, number>([]),
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
    ['Reset']: (data: Messages, action: Action) => []
});

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
    ownedStock: new SimpleMap<ChainName, number>([]),
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
                    // DONE send to backend: sendCommand model List
                    return {type: 'SelectGame', player: game.player, games: [], numPlayers: 1, numRobots: 5};
            }
            return game;

        case 'SelectGame':
            switch (action.type) {
                case 'SetNumPlayers': {
                    const number: number = parseInt(action.num, 10);
                    if (isNaN(number)) {
                        return game;
                    }
//                    return Object.assign({}, game, {numPlayers: number});
                    return {
                        type: game.type,
                        player: game.player,
                        games: game.games,
                        numPlayers: number,
                        numRobots: game.numRobots
                    };
                }
                case 'SetNumRobots': {
                    const number: number = parseInt(action.num, 10);
                    if (isNaN(number)) {
                        return game;
                    }
                    return {
                        type: game.type,
                        player: game.player,
                        games: game.games,
                        numPlayers: game.numPlayers,
                        numRobots: number
                    };
                }
                case 'CreateGame':
                    // DONE send to backend: sendCommand model (NewGame { numHumans = sg.numPlayers, numRobots = sg.numRobots })
                    return game;
                case 'Join':
                    // DONE send to backend: sendCommand model (JoinGame { playerName = sg.player.playerName, gameId = g })
                    return game;

                // from backend:
                case 'GamesList':
                    return {
                        type: game.type,
                        player: game.player,
                        games: action.games,
                        numPlayers: game.numPlayers,
                        numRobots: game.numRobots
                    };
                case 'GameStarts':
                    return {
                        type: 'PlayGame'
                        , player: game.player
                        , gameId: action.gameId
                        , board: new SimpleMap<Tile, Cell>([])
                        , possiblePlays: []
                        , highlightedCell: null                    }
            }
            return game;

        case 'PlayGame':
            switch (action.type) {
                case 'Play':
                    // DONE send to backend: sendCommand model (Action { selectedPlay = n })
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possiblePlays: [],
                        highlightedCell: game.highlightedCell
                    };
                case 'HighlightCell':
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possiblePlays: game.possiblePlays,
                        highlightedCell: action.tile
                    };
                case 'UnhighlightCell':
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possiblePlays: game.possiblePlays,
                        highlightedCell: null
                    };
                case 'GameUpdated':
                    return {
                        type: game.type,
                        player: action.player,
                        gameId: game.gameId,
                        board: action.board,
                        possiblePlays: action.possiblePlays,
                        highlightedCell: game.highlightedCell
                    };
            }
            return game;

        case 'EndOfGame':
            switch (action.type) {
                case 'Reset':
                    // DONE send to backend: sendCommand model List
                    return {
                        type: 'SelectGame',
                        player: game.player,
                        games: [],
                        numPlayers: 1,
                        numRobots: 5
                    };
            }
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
