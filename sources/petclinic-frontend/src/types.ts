
export interface Pet {
    name: string,
    species: string
}

export interface Model {
    admittedPets: Pet[]
}


export interface Props {
    model: Model,
    dispatch?: any
}
