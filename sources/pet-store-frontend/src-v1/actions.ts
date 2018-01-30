import * as ajax from './ajaxcalls';

import {Pet} from './types';


export type PetFromBackend = { petName: string, petType: string }
export type PetStoreErrorFromBackend = {}

export type DataFromBackend
// events:
    = { tag: 'PetAdded', pet: PetFromBackend }
    | { tag: 'PetRemoved', pet: PetFromBackend }
// answers:
    | { tag: 'Pets', pets: PetFromBackend[] }
    | { tag: 'Error', reason: PetStoreErrorFromBackend }


const transformPet = (petFromBackend: PetFromBackend) => ({
    name: petFromBackend.petName,
    species: petFromBackend.petType
});

const transformPets = (petsFromBackend: PetFromBackend[]) => petsFromBackend.map(transformPet);

export function transformBackendDataToAction(dataFromBackend: DataFromBackend): Action {
    switch (dataFromBackend.tag) {
        case 'PetAdded':
            return addPet(transformPet(dataFromBackend.pet));
        case 'PetRemoved':
            return sellPet(transformPet(dataFromBackend.pet));
        case 'Pets':
            return updatePets(transformPets(dataFromBackend.pets));
        case 'Error':
            return {type: 'InitialAction'};
    }
}


///////////////////////////////////////////////////////////////////////////////////////
// ACTIONS

export type Action
    // frontend-only actions (events):
    = { type: 'InitialAction' }
    | { type: 'PETS_LOADED', pets: Pet[] }

    // frontend actions (events) that correspond to frontend-backend commands:
    | { type: 'PET_ADDED', addedPet: Pet }
    | { type: 'PET_SOLD', soldPet: Pet }

// backend events:


export function updatePets(pets: Pet[]): Action {
    return {
        type: 'PETS_LOADED',
        pets: pets
    };
}

export function loadPets() {
    return (dispatch: any) => {
        ajax.fetchPets((d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}

export function addPet(pet: Pet): Action {
    return {
        type: 'PET_ADDED',
        addedPet: pet
    };
}


export function addAndSubmitPet(pet: Pet) {
    return (dispatch: any) => {

        // dispatch(addPet(pet));

        ajax.submitPet(pet, (d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}


export function sellPet(pet: Pet): Action {
    return {
        type: 'PET_SOLD',
        soldPet: pet
    };
}

export function sellAndSubmitPet(pet: Pet) {
    return (dispatch: any) => {

        // dispatch(sell(pet));

        ajax.sellPet(pet, (d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}
