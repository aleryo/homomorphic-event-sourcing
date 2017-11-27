import {Game, GameBoard, GameDescription, GameId, Order, Player, PlayerName, Tile, URL} from './types';

export type Action
    = { type: 'InitialAction' }

    // frontend-only actions:
    | { type: 'SetName', name: string }
    | { type: 'RegisterPlayer' }
    | { type: 'SetNumPlayers', num: string }
    | { type: 'SetNumRobots', num: string }

    | { type: 'CreateGame' }

    | { type: 'JoinGame', gameDescId: GameId }
    | { type: 'Move', move: number }

    | { type: 'ShowMessages' }
    | { type: 'HideMessages' }
    | { type: 'HighlightCell', tile: Tile }
    | { type: 'UnhighlightCell' }
    | { type: 'Reset' }

    // frontend -> backend:
    // List
    // CreateGame
    // JoinGame
    // Action
    // Bye

    // middleware -> frontend: (emitted from the middleware on reception of a backend message)
    | { type: 'PlayerRegistered', playerName: PlayerName, gameId: GameId } // is not handled in reducers, only triggers loading of games list
    | { type: 'GamesListed', games: GameDescription[] }
    | { type: 'NewGameCreated', gameId: GameId } // not handled in reducers
    | { type: 'GameStarted', gameId: GameId }
    | { type: 'GameUpdated', board: GameBoard, possibleMoves: Order[], player: Player }
    | { type: 'ErrorMessage', message: string }
    // TODO playerName is not used anywhere:
    | { type: 'Played', playerName: PlayerName, board: GameBoard, played: Order }
    | { type: 'GameEnds', endGame: Game }

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
    return {type: 'JoinGame', gameDescId};
}

export function move(move: number): Action {
    return {type: 'Move', move};
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
