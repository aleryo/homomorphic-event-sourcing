import {GameId, Tile} from './types';


export type Action
    = { kind: 'Output', output: string }
    | { kind: 'UseKey', key: string }
    | { kind: 'SetName', name: string }
    | { kind: 'RegisterPlayer' }
    | { kind: 'ListGames' }
    | { kind: 'Join', gameDescId: GameId }
    | { kind: 'CreateGame' }
    | { kind: 'Play', move: number }
    | { kind: 'SetNumPlayers', num: string }
    | { kind: 'SetNumRobots', num: string }
    | { kind: 'ShowMessages' }
    | { kind: 'HideMessages' }
    | { kind: 'HighlightCell', tile: Tile }
    | { kind: 'UnhighlightCell' }
    | { kind: 'Reset' }

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
