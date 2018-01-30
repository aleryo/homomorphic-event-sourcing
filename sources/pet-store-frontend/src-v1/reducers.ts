
import {Model, Pet} from './types';
import {combineReducers} from 'redux';
import * as ajax from "./ajaxcalls";
import {Action} from './actions';

function createReducer(initialState : any, handlers : any) {
    return (state : any = initialState, action : Action = { type: 'InitialAction' }) =>
        handlers.hasOwnProperty(action.type) ?
            handlers[action.type](state, action) :
            state
}


///////////////////////////////////////////////////////////////////////////////////////
// State

export const INITIAL_STATE: Model = {
    pets: []
};


const pets = createReducer(INITIAL_STATE.pets, {
    ["PETS_LOADED"]: (currentPets:Pet[], action:{ type: "PETS_LOADED", pets: Pet[] }) => action.pets,
    ["PET_ADDED"]: (currentPets:Pet[], action:{ type: "PET_ADDED", addedPet: Pet }) => currentPets.concat(action.addedPet),
    ["PET_SOLD"]:  (currentPets:Pet[], action:{ type: "PET_SOLD", soldPet: Pet }) =>
        currentPets.filter((pet:Pet) => pet.name !== action.soldPet.name || pet.species !== action.soldPet.species)
});


export default combineReducers({
    pets
});
