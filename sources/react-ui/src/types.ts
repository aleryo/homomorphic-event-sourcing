//import {Map} from "core-js";

import * as R from 'ramda';

export class SimpleMap<K,V>{
    elems: {key:K, value:V}[];

    constructor(){
        this.elems = [];
    }

    values(){
        return R.pluck('value')(this.elems);
    }
}

export type Tile = { label: string, value: number }

export type ChainName = String

export type Content
    = { kind: 'Empty' }
    | { kind: 'Playable' } // -- ^Used for highlighting purpose
    | { kind: 'Neutral', tile: Tile }
    | { kind: 'Chain', chainName: ChainName }

export interface Cell {
    cellCoord: Tile
    ,
    cellContent: Content
}

export type Stock = SimpleMap<ChainName, number>
//    Dict.Dict ChainName Int

export type PlayerName = string

export enum PlayerType {
    Human,
    Robot
}

export interface Player {
    playerName: PlayerName
    ,
    playerType: PlayerType
    ,
    tiles: Tile[]
    ,
    ownedStock: Stock
    ,
    ownedCash: number
}

export type Players = SimpleMap<PlayerName, Player>;
//    Dict.Dict PlayerName Player


// -- Hotels


export interface HotelChain {
    chainName: ChainName
    ,
    chainTiles: Tile[]
    ,
    chainStock: number
}


export type HotelChains = SimpleMap<ChainName, HotelChain>;
//    Dict.Dict ChainName HotelChain


// -- Orders


export type MergerPhase
    = { kind: 'TakeOver', tile: Tile, chainNames: ChainName[] }
    | {
    kind: 'DisposeStock'
    , initialPlayer: PlayerName
    , buyerChain: ChainName
    , buyeeChain: ChainName
    , buyeePrice: number
    , playersToDecide: PlayerName[]
}


export type Phase
    = { kind: 'PlaceTile' }
    | { kind: 'FundChain', tile: Tile }
    | { kind: 'BuySomeStock', amount: number }
    | { kind: 'ResolveMerger', phase: MergerPhase, turn: Turn }
    | { kind: 'GameEnding' }


export type Turn =
    { name: PlayerName, phase: Phase }


export type Order
    = { kind: 'Place', playerName: PlayerName, tile: Tile }
    | { kind: 'Merge', playerName: PlayerName, tile: Tile, fromChain: ChainName, toChain: ChainName }
    | { kind: 'Fund', playerName: PlayerName, chainName: ChainName, tile: Tile }
    | { kind: 'BuyStock', playerName: PlayerName, chainName: ChainName }
    | { kind: 'SellStock', playerName: PlayerName, chainName: ChainName, amount: number, todo: number } // FIXME param name
    | { kind: 'ExchangeStock', playerName: PlayerName, fromChain: ChainName, toChain: ChainName, amount: number }
    | { kind: 'Pass' }
    | { kind: 'EndGame' }
    | { kind: 'Cancel' }


export type GameId = string

export type GameBoard = SimpleMap<Tile, Cell>
//    Dict.Dict Tile Cell

export interface Game {
    gameId: GameId
    ,
    gameBoard: GameBoard
    ,
    players: Players
}

export type GameDescription =
    {
        gameDescId: GameId
        , descNumberOfHumans: number
        , descNumberOfRobots: number
        , descRegisteredHumans: PlayerName[]
        , descLive: boolean
    }

/*
export type GameState = { gameType: "Register", player: Player }
| { gameType: "SelectGame", player: Player, games: GameDescription[], numPlayers: number, numRobots: number }
| { gameType: "PlayGame", player: Player, gameId: GameId }
| { gameType: "EndOfGame", player: Player, gameId: GameId, gameResult: any }
*/

export type GameState
    = { gameType: 'Register', player: Player }
    | { gameType: 'SelectGame', player: Player, games: GameDescription[], numPlayers: number, numRobots: number }
    | {
    gameType: 'PlayGame'
    , player: Player
    , gameId: GameId
    , board: GameBoard
    , possiblePlays: Order[]
    , highlightedCell?: Tile
}
    | { gameType: 'EndOfGame', player: Player, gameId: GameId, board: GameBoard, gameResult: Players }


export type Domain = { host: string, port: string }

export interface Model {
    strings: string[]
    ,
    showMessages: boolean
    ,
    errors: string[]
    ,
    domain: Domain
    ,
    wsServerUrl: string
    ,
    game: GameState
}


export interface Props {
    model: Model,
    dispatch?: any
}
