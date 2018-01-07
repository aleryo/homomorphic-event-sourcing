
import {Model, Pet} from './types';
import {combineReducers} from 'redux';

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

    // frontend-backend actions:
    | { type: "PET_ADMITTED", pet: Pet }


export function admitPet(pet:Pet) : Action {
    return {
        type: "PET_ADMITTED",
        pet: pet
    }
}

///////////////////////////////////////////////////////////////////////////////////////
// State

export const INITIAL_STATE: Model = {
    admittedPets: []
};



const admittedPets = createReducer(INITIAL_STATE.admittedPets, {
    ["PET_ADMITTED"]: (currentPets:Pet[], action:{ type: "PET_ADMITTED", pet: Pet }) => currentPets.concat(action.pet)
});


export default combineReducers({
    admittedPets
});
