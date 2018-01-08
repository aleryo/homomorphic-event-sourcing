
export interface Pet {
    name: string,
    species: string
}

export interface Model {
    admittedPets: Pet[]
}


export interface Props extends Model {
    dispatch?: any
}
