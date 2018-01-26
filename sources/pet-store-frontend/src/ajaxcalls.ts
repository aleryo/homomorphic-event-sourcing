const axios = require('axios');

import {Pet} from './types';

interface PetFromBackend {
    petName: string,
    petType: string
}

export function submitPet(pet: Pet, callback: any, url: string = '') {
    axios.post(url + '/pets',
        {
            petName: pet.name,
            petType: pet.species,
            petPrice: 1
        }).then(
        (response: any) => {
            // callback(response.data);
        }
    );
}

function transformPets(dataFromBackend: any): Pet[] {
    return dataFromBackend.pets.map((petFromBackend: PetFromBackend) => ({
        name: petFromBackend.petName,
        species: petFromBackend.petType
    }));
}

export function fetchPets(successCB: any, url: string = '') {
    axios.get(url + '/pets', {
        headers: {'Accept': 'application/json'},
    }).then(
        (response: any) => {
            successCB(transformPets(response.data));
        },
        (error: string) => {
            console.log('houston...', error);
        });
}
