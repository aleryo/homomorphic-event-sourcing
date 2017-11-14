import {GameId, Tile} from './types';

''
export type Action
    = { type: 'Output', output: string } // TODO
    | { type: 'UseKey', key: string } // TODO
    | { type: 'SetName', name: string } // TODO
    | { type: 'RegisterPlayer' } // TODO
    | { type: 'ListGames' } // TODO
    | { type: 'Join', gameDescId: GameId } // TODO
    | { type: 'CreateGame' } // TODO
    | { type: 'Play', move: number } // TODO
    | { type: 'SetNumPlayers', num: string } // TODO
    | { type: 'SetNumRobots', num: string } // TODO
    | { type: 'ShowMessages' }
    | { type: 'HideMessages' }
    | { type: 'HighlightCell', tile: Tile } // TODO
    | { type: 'UnhighlightCell' } // TODO
    | { type: 'Reset' } // TODO

// Output
// UseKey

export function setName(name: string): Action {
    return {type: 'SetName', name};
}

export function registerPlayer(): Action {
    return {type: 'RegisterPlayer'};
}

// ListGames

export function joinGame(gameDescId: GameId): Action {
    return {type: 'Join', gameDescId};
}

export function createGame(): Action {
    return {type: 'CreateGame'};
}

export function play(move: number): Action {
    return {type: 'Play', move};
}

export function setNumPlayers(num: string): Action {
    return {type: 'SetNumPlayers', num};
}

export function setNumRobots(num: string): Action {
    return {type: 'SetNumRobots', num};
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
