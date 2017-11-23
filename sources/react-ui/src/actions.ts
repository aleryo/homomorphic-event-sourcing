import {Game, GameBoard, GameDescription, GameId, Order, Player, PlayerName, Tile, URL} from './types';

export type Action
    = { type: 'InitialAction' }

    // frontend-only actions:
    | { type: 'SetName', name: string }
    | { type: 'RegisterPlayer' }
    | { type: 'SetNumPlayers', num: string }
    | { type: 'SetNumRobots', num: string }

    | { type: 'CreateGame' }

    | { type: 'Join', gameDescId: GameId }
    | { type: 'Play', move: number }

    | { type: 'ShowMessages' }
    | { type: 'HideMessages' }
    | { type: 'HighlightCell', tile: Tile }
    | { type: 'UnhighlightCell' }
    | { type: 'Reset' }

    // frontend -> backend:
    // List
    // NewGame
    // JoinGame
    // Action
    // Bye

    // middleware -> frontend: (emitted from the middleware on reception of a backend message)
    | { type: 'PlayerRegistered', playerName: PlayerName, gameId: GameId } // TODO handle in reducers
    | { type: 'GamesList', games: GameDescription[] }
    | { type: 'NewGameStarted', gameId: GameId } // not handled in reducers
    | { type: 'GameStarts', gameId: GameId }
    | { type: 'GameUpdated', board: GameBoard, possiblePlays: Order[], player: Player }
    | { type: 'ErrorMessage', message: string } // TODO handle in reducers
    | { type: 'Played', playerName: PlayerName, board: GameBoard, played: Order } // TODO handle in reducers
    | { type: 'GameEnds', endGame: GameBoard } // TODO handle in reducers

    // ???
    | { type: 'Output', output: string } // TODO
    | { type: 'UseKey', key: string } // TODO

    // technical stuff for webservices
    | { type: 'ConnectWS', url: URL }


// Output
// UseKey

export function setName(name: string): Action {
    return {type: 'SetName', name};
}

export function registerPlayer(): Action {
    return {type: 'RegisterPlayer'};
}

export function setNumPlayers(num: string): Action {
    return {type: 'SetNumPlayers', num};
}

export function setNumRobots(num: string): Action {
    return {type: 'SetNumRobots', num};
}

export function createGame(): Action {
    return {type: 'CreateGame'};
}

export function joinGame(gameDescId: GameId): Action {
    return {type: 'Join', gameDescId};
}

export function play(move: number): Action {
    return {type: 'Play', move};
}

export function showMessages(): Action {
    return {type: 'ShowMessages'};
}

export function hideMessages(): Action {
    return {type: 'HideMessages'};
}

export function highlightCell(tile: Tile): Action {
    return {type: 'HighlightCell', tile};
}

export function unhighlightCell(): Action {
    return {type: 'UnhighlightCell'};
}

export function reset(): Action {
    return {type: 'Reset'};
}

// Actions that talk to the backend
