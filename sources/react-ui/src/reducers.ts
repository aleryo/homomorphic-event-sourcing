import {combineReducers} from 'redux';
import * as R from 'ramda';

import {Cell, ChainName, Errors, GameState, Messages, Model, Order, Player, PlayerType, SimpleMap, Tile} from './types';
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
        strings: []
        , displayMessages: true
        , errors: []
        , domain: {host: '0', port: '0'}
        , wsServerUrl: ''
        , game: {type: 'Register', player: INITIAL_PLAYER}
    };


function strings(strings: Errors = INITIAL_STATE.strings, action: Action = {type: 'InitialAction'}) {
    switch(action.type){
        case 'Reset':
            return [];
        case 'Played':
            return R.prepend(showOrder(action.played), strings);
    }
    return strings;
}

const displayMessages = createReducer(INITIAL_STATE.displayMessages, {
    ['ShowMessages']: (data: boolean, action: Action) => true,
    ['HideMessages']: (data: boolean, action: Action) => false
});

function errors(errors: Errors = INITIAL_STATE.errors, action: Action = {type: 'InitialAction'}) {
    switch(action.type){
        case 'ErrorMessage':
            return R.prepend(action.message, errors);
    }
    return errors;
}

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

const showTile = (tile:Tile) : string => tile.row + "-" + tile.col

const showOrder = (order:Order) : string => {
    switch (order.tag) {
        case 'Place':
            return order.playerName + " plays @" + showTile(order.tile);
        case 'Merge':
            return order.playerName + " merges " + order.fromChain + " into " + order.toChain + " @" + showTile(order.tile);
        case 'Fund':
            return order.playerName + " founds " + order.chainName + " @" + showTile(order.tile);
        case 'BuyStock':
            return order.playerName + " buys 1 share of " + order.chainName;
        case 'SellStock':
            return order.playerName + " sells "
                + order.amount + " share" + (order.amount > 1 ? "s" : "")
                + " of " + order.chainName + " at " + order.todo + "$";
        case 'ExchangeStock':
            return order.playerName + " exchanges "
                + order.amount + "share" + (order.amount > 1 ? "s" : "")
                + " of " + order.fromChain + " against "
                + (order.amount/2) + " share" + ((order.amount/2) > 1 ? "s" : "")
                + " of " + order.toChain;
        case 'Pass':
            return "pass";
        case 'EndGame':
            return "end game";
        case 'Cancel':
            return "cancel";
    }
};



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
                case 'GamesListed':
                    return {
                        type: game.type,
                        player: game.player,
                        games: action.games,
                        numPlayers: game.numPlayers,
                        numRobots: game.numRobots
                    };
                case 'GameStarted':
                    return {
                        type: 'PlayGame'
                        , player: game.player
                        , gameId: action.gameId
                        , board: new SimpleMap<Tile, Cell>([])
                        , possibleMoves: []
                        , highlightedCell: null                    }
            }
            return game;

        case 'PlayGame':
            switch (action.type) {
                case 'Move':
                    // DONE send to backend: sendCommand model (Action { selectedPlay = n })
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possibleMoves: [],
                        highlightedCell: game.highlightedCell
                    };
                case 'HighlightCell':
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possibleMoves: game.possibleMoves,
                        highlightedCell: action.tile
                    };
                case 'UnhighlightCell':
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: game.board,
                        possibleMoves: game.possibleMoves,
                        highlightedCell: null
                    };
                case 'GameUpdated':
                    return {
                        type: game.type,
                        player: action.player,
                        gameId: game.gameId,
                        board: action.board,
                        possibleMoves: action.possibleMoves,
                        highlightedCell: game.highlightedCell
                    };
                case 'Played':
                    return {
                        type: game.type,
                        player: game.player,
                        gameId: game.gameId,
                        board: action.board,
                        possibleMoves: [],
                        highlightedCell: game.highlightedCell
                    };
                case 'GameEnds':
                    // TODO compare action.endGame.gameId??
                    return { type: 'EndOfGame',
                        player: game.player,
                        gameId: game.gameId,
                        board: action.endGame.gameBoard,
                        gameResult: action.endGame.players
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
