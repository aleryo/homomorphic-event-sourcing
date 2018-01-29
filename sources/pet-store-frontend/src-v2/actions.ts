
import * as ajax from "./ajaxcalls";

import {Pet} from './types';

export type Action
    // frontend-only actions (events):
    = { type: 'InitialAction' }
    | { type: "PETS_LOADED", pets: Pet[] }

    // frontend actions (events) that correspond to frontend-backend commands:
    | { type: "PET_ADDED", addedPet: Pet }
    | { type: "PET_SOLD", soldPet: Pet }
    | { type: "USER_LOGGED_IN", addedUser: string }
    | { type: "USER_LOGGED_OUT" }

// backend events:


///////////////////////////////////////////////////////////////////////////////////////
// ACTIONS

export function loadPets() {
    return (dispatch:any) => {
        ajax.fetchPets((pets:Pet[]) => {
            dispatch(updatePets(pets));
        });
    };
}

export function updatePets(pets:Pet[]) : Action {
    return {
        type: "PETS_LOADED",
        pets: pets
    }
}

export function addPet(pet:Pet) : Action {
    return {
        type: "PET_ADDED",
        addedPet: pet
    }
}


export function addAndSubmitPet(pet:Pet) {
    return (dispatch:any) => {

        dispatch(addPet(pet));

        ajax.submitPet(pet,(pets:Pet[]) => {
            // dispatch(updatePets(pets));
        });
    };
}


export function sellPet(pet:Pet) : Action {
    return {
        type: "PET_SOLD",
        soldPet: pet
    }
}

export function loginUser(user:string) : Action {
    return {
        type: "USER_LOGGED_IN",
        addedUser: user
    }
}

export function loginAndSubmitUser(user:string) {
    return (dispatch:any) => {

        dispatch(loginUser(user));

        ajax.submitLogin(user, dispatch);
    };
}

export function logoutUser() : Action {
    return { type: "USER_LOGGED_OUT" };
}

export function logoutAndSubmitUser() {
    return (dispatch:any, state:any) => {

        const {user} = state;

        dispatch(logoutUser());

        ajax.submitLogout(user,() => {
            // dispatch(updatePets(pets));
        });
    };
}
