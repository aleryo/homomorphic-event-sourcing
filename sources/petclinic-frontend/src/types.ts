
export interface Pet {
    name: string,
    species: string
}

export interface Model {
    pets: Pet[]
}


export interface Props extends Model {
    dispatch?: any
}
