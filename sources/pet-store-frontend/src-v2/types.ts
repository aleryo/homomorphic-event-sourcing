
export interface Pet {
    name: string,
    species: string,
    price: number
}

export interface Model {
    pets: Pet[]
}


export interface Props extends Model {
    dispatch?: any
}
