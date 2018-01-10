
import {Model, Pet} from './types';
import {combineReducers} from 'redux';
import * as ajax from "./ajaxcalls";

function createReducer(initialState : any, handlers : any) {
    return (state : any = initialState, action : Action = { type: 'InitialAction' }) =>
        handlers.hasOwnProperty(action.type) ?
            handlers[action.type](state, action) :
            state
}

///////////////////////////////////////////////////////////////////////////////////////
// ACTIONS

export type Action
    // frontend-only actions:
    = { type: 'InitialAction' }
    | { type: "PETS_LOADED", pets: Pet[] }

    // frontend-backend actions:
    | { type: "PET_ADDED", addedPet: Pet }
    | { type: "PET_SOLD", soldPet: Pet }

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

export function sellPet(pet:Pet) : Action {
    return {
        type: "PET_SOLD",
        soldPet: pet
    }
}

///////////////////////////////////////////////////////////////////////////////////////
// State

export const INITIAL_STATE: Model = {
    pets: []
};



const pets = createReducer(INITIAL_STATE.pets, {
    ["PET_ADDED"]: (currentPets:Pet[], action:{ type: "PET_ADDED", addedPet: Pet }) => currentPets.concat(action.addedPet),
    ["PET_SOLD"]:  (currentPets:Pet[], action:{ type: "PET_SOLD", soldPet: Pet }) =>
        currentPets.filter((pet:Pet) => pet.name !== action.soldPet.name && pet.species !== action.soldPet.species)
});


export default combineReducers({
    pets
});
