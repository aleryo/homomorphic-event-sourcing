
export interface Pet {
    name: string,
    species: string
}

export interface Error {
    message: string
}

export interface Model {
    pets: Pet[],
    error: Error | null
}


export interface Props extends Model {
    dispatch?: any
}
