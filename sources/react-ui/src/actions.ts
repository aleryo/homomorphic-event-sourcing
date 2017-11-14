import {GameId, Tile} from './types';

''
export type Action
    = { kind: 'Output', output: string } // TODO
    | { kind: 'UseKey', key: string } // TODO
    | { kind: 'SetName', name: string } // TODO
    | { kind: 'RegisterPlayer' } // TODO
    | { kind: 'ListGames' } // TODO
    | { kind: 'Join', gameDescId: GameId } // TODO
    | { kind: 'CreateGame' } // TODO
    | { kind: 'Play', move: number } // TODO
    | { kind: 'SetNumPlayers', num: string } // TODO
    | { kind: 'SetNumRobots', num: string } // TODO
    | { kind: 'ShowMessages' }
    | { kind: 'HideMessages' }
    | { kind: 'HighlightCell', tile: Tile } // TODO
    | { kind: 'UnhighlightCell' } // TODO
    | { kind: 'Reset' } // TODO

// Output
// UseKey

export function setName(name: string): Action {
    return {kind: 'SetName', name};
}

export function registerPlayer(): Action {
    return {kind: 'RegisterPlayer'};
}

// ListGames

export function joinGame(gameDescId: GameId): Action {
    return {kind: 'Join', gameDescId};
}

export function createGame(): Action {
    return {kind: 'CreateGame'};
}

export function play(move: number): Action {
    return {kind: 'Play', move};
}

export function setNumPlayers(num: string): Action {
    return {kind: 'SetNumPlayers', num};
}

export function setNumRobots(num: string): Action {
    return {kind: 'SetNumRobots', num};
}

export function showMessages(): Action {
    return {kind: 'ShowMessages'};
}

export function hideMessages(): Action {
    return {kind: 'HideMessages'};
}

export function highlightCell(tile: Tile): Action {
    return {kind: 'HighlightCell', tile};
}

export function unhighlightCell(): Action {
    return {kind: 'UnhighlightCell'};
}

export function reset(): Action {
    return {kind: 'Reset'};
}
