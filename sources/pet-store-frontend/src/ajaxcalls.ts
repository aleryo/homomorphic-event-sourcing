import ajax from 'nanoajax';
import {Pet} from './types';

interface PetFromBackend {
    petName: string,
    petType: string
}

export function submitPet(pet: Pet, callback: any) {
    ajax.ajax({
            url: '/pets',
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
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

export function fetchPets(successCB: any) {
    ajax.ajax({
            url: '/pets',
            method: 'GET'
        },
        (code: number, response: string) => {
        if(code === 200){
            successCB(transformPets(JSON.parse(response)));
        } else {
            // TODO error handling
        }
        }
    );
}
