import * as ajax from './ajaxcalls';

import {Pet} from './types';


export type PetFromBackend = { petName: string, petType: string, petPrice: number }
export type UserFromBackend = { userName: string }
export type PaymentFromBackend = {}
export type PetStoreErrorFromBackend = {}

export type DataFromBackend
// events:
    = { tag: 'PetAdded', pet: PetFromBackend }
    | { tag: 'PetRemoved', pet: PetFromBackend }
    | { tag: 'UserLoggedIn', user: UserFromBackend }
    | { tag: 'AddedToBasket', user: UserFromBackend, pet: PetFromBackend }
    | { tag: 'RemovedFromBasket', user: UserFromBackend, pet: PetFromBackend }
    | { tag: 'CheckedOutBasket', user: UserFromBackend, payment: PaymentFromBackend, amount: number }
    | { tag: 'UserLoggedOut', user: UserFromBackend }
// answers:
    | { tag: 'UserBasket', user: UserFromBackend, pets: PetFromBackend[] }
    | { tag: 'Pets', pets: PetFromBackend[] }
    | { tag: 'Error', reason: PetStoreErrorFromBackend }


const transformPet = (petFromBackend: PetFromBackend) => ({
    name: petFromBackend.petName,
    species: petFromBackend.petType,
    price: petFromBackend.petPrice
});

const transformPets = (petsFromBackend: PetFromBackend[]) => petsFromBackend.map(transformPet);

const transformUser = (userFromBackend: UserFromBackend) => userFromBackend.userName;

export function transformBackendDataToAction(dataFromBackend: DataFromBackend): Action {
    switch (dataFromBackend.tag) {
        case 'PetAdded':
            return addPet(transformPet(dataFromBackend.pet));
        case 'PetRemoved':
            return {type: 'InitialAction'};
        case 'UserLoggedIn':
            return loginUser(transformUser(dataFromBackend.user));
        case 'AddedToBasket':
            return {type: 'InitialAction'};
        case 'RemovedFromBasket':
            return {type: 'InitialAction'};
        case 'CheckedOutBasket':
            return {type: 'InitialAction'};
        case 'UserLoggedOut':
            return logoutUser();
        case 'UserBasket':
            return {type: 'InitialAction'};
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
    | { type: 'USER_LOGGED_IN', addedUser: string }
    | { type: 'USER_LOGGED_OUT' }

// backend events:


export function loadPets() {
    return (dispatch: any) => {
        ajax.fetchPets((d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}

export function updatePets(pets: Pet[]): Action {
    return {
        type: 'PETS_LOADED',
        pets: pets
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

export function loginUser(user: string): Action {
    return {
        type: 'USER_LOGGED_IN',
        addedUser: user
    };
}

export function loginAndSubmitUser(user: string) {
    return (dispatch: any) => {

        dispatch(loginUser(user));

        ajax.submitLogin(user, (d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}

export function logoutUser(): Action {
    return {type: 'USER_LOGGED_OUT'};
}

export function logoutAndSubmitUser() {
    return (dispatch: any, state: any) => {

        const {user} = state;

        dispatch(logoutUser());

        ajax.submitLogout(user, (d: DataFromBackend) => dispatch(transformBackendDataToAction(d)));
    };
}
