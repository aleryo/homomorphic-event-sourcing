
export type PlayerName = string

export interface Player {
    playerName: PlayerName,
    ownedCash: number,
    playerType: string
}

export interface GameId {
    id: string
}

export type GameDescription =
    { gameDescId : GameId
    , descNumberOfHumans : number
    , descNumberOfRobots : number
    , descRegisteredHumans : PlayerName[]
    , descLive : boolean
    }

export type GameState = { gameType: "Register", player: Player }
    | { gameType: "SelectGame", player: Player, games: GameDescription[], numPlayers: number, numRobots: number }
    | { gameType: "PlayGame", player: Player, gameId: GameId }
    | { gameType: "EndOfGame", player: Player, gameId: GameId, gameResult: any }


export interface Model {
    game: GameState,
    showMessages: boolean,
    strings: string[],
    errors: string[]
}


export interface Props {
    model: Model,
    dispatch?: any
}
