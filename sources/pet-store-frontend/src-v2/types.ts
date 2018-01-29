
export interface Pet {
    name: string,
    species: string,
    price: number
}

export interface Model {
    pets: Pet[],
    user: string | null
}


export interface Props extends Model {
    dispatch?: any
}
