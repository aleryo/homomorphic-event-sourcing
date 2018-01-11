import ajax from "nanoajax";
import {Pet} from './types';

export function submitPet(pet:Pet, callback:any) {
    ajax.ajax({
            url: "/pets",
            method: "POST",
            headers: {"Content-Type": "application/json"},
            body: JSON.stringify({petName: pet.name, petType: pet.species})
        },
        (code:number, response:string) => {
            // callback(JSON.parse(response));
        }
    );
}

export function fetchPets(callback:any) {
    ajax.ajax({
            url: "/pets",
            method: "GET"
        },
        (code:number, response:string) => {
            callback(JSON.parse(response));
        }
    );
}
