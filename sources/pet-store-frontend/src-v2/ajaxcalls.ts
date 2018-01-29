import {Action, loginUser, logoutUser} from './actions';

const axios = require('axios');

import {Pet} from './types';

interface PetFromBackend {
    petName: string,
    petType: string,
    petPrice: number
}

export function submitPet(pet: Pet, callback: any, url: string = '') {
    axios.post(url + '/pets',
        {
            petName: pet.name,
            petType: pet.species,
            petPrice: pet.price
        }).then(
        (response: any) => {
            // callback(response.data);
        }
    ).catch(() => { console.log("ERROR!"); });
}

function transformPets(dataFromBackend: any): Pet[] {
    return dataFromBackend.pets.map((petFromBackend: PetFromBackend) => ({
        name: petFromBackend.petName,
        species: petFromBackend.petType,
        price: petFromBackend.petPrice
    }));
}

export function fetchPets(successCB: any, url: string = '') {
    axios.get(url + '/pets', {
        headers: {'Accept': 'application/json'},
    }).then(
        (response: any) => {
            successCB(transformPets(response.data));
        }).catch(
        (error: string) => {
            console.log('houston...', error);
        });
}

function transformLogin(dataFromBackend: any): Action {
    if(dataFromBackend.tag === "UserLoggedIn"){
        // {"tag":"UserLoggedIn","user":{"userName":"Nicole"}}
        return loginUser(dataFromBackend.user.userName);
    } else {
        return logoutUser();
    }
}

export function submitLogin(user: string, successCB: ((action:Action) => void), url: string = '') {
    axios.put(url + '/users/' + user).then(
        (response: any) => {
            successCB(transformLogin(response.data));
        }
    ).catch(() => { console.log("ERROR!"); successCB(logoutUser()); });
}

function transformLogout(dataFromBackend: any): Action {
    if(dataFromBackend.tag === "UserLoggedOut"){
        // {"tag":"UserLoggedIn","user":{"userName":"Nicole"}}
        return logoutUser();
    } else {
        return logoutUser(); // TODO FIXME
    }
}

export function submitLogout(user: string, successCB: ((action:Action) => void), url: string = '') {
    axios.delete(url + '/users/' + user).then(
        (response: any) => {
            successCB(transformLogout(response.data));
        }
    ).catch(() => { console.log("ERROR!"); });
}

