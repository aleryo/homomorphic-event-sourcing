// import ajax from 'nanoajax';
// const ajax = require('nanoajax');
const axios = require('axios');

import {Pet} from './types';

interface PetFromBackend {
    petName: string,
    petType: string
}

export function submitPet(pet: Pet, callback: any, url: string = "") {
    axios.request({
            // baseURL: url,
            url: url + '/pets',
            method: 'POST',
            headers: {'Content-Type': 'application/json;charset=utf-8'},
            body: JSON.stringify({petName: pet.name, petType: pet.species})
        },
        (code: number, response: string) => {
            // callback(JSON.parse(response));
        }
    );
}

function transformPets(petsFromBackend: PetFromBackend[]) : Pet[] {
    return petsFromBackend.map((petFromBackend:PetFromBackend) => ({name: petFromBackend.petName, species: petFromBackend.petType}) );
}

export function fetchPets(successCB: any, url: string = "") {
    axios.request({
        method: 'GET',
        // baseURL: url,
        url: url + '/pets',
        headers: {'Accept': 'application/json;charset=utf-8'},
    }).then(
        (response: any) => {
            console.log("success", response.data)
            successCB(transformPets(response.data));
        },
        (error: string) => {
            console.log('houston...', error);
        });
}
