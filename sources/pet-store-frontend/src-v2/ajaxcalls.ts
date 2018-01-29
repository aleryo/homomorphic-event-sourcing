import {DataFromBackend} from './actions';
import {Pet} from './types';

const axios = require('axios');


export function submitPet(pet: Pet, successCB: ((d: DataFromBackend) => void), url: string = '') {
    axios.post(url + '/pets',
        {
            petName: pet.name,
            petType: pet.species,
            petPrice: pet.price
        })
        .then((response: { data: DataFromBackend }) => successCB(response.data))
        .catch((error: string) => console.log('ERROR!', error));
}


export function fetchPets(successCB: any, url: string = '') {
    axios.get(url + '/pets', {headers: {'Accept': 'application/json'}})
        .then((response: { data: DataFromBackend }) => successCB(response.data))
        .catch((error: string) => console.log('ERROR!', error));
}

export function submitLogin(user: string, successCB: ((d: DataFromBackend) => void), url: string = '') {
    axios.put(url + '/users/' + user)
        .then((response: { data: DataFromBackend }) => successCB(response.data))
        .catch((error: string) => console.log('ERROR!', error));
}

export function submitLogout(user: string, successCB: ((d: DataFromBackend) => void), url: string = '') {
    axios.delete(url + '/users/' + user)
        .then((response: { data: DataFromBackend }) => successCB(response.data))
        .catch((error: string) => console.log('ERROR!', error));
}

