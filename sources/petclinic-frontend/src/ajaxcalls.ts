import ajax from "nanoajax";
import {Pet} from './types';

export function addPet(pet:Pet, callback:any) {
    ajax.ajax({
            url: "/api/addPet",
            method: "POST",
            body: "name=" + pet.name + "&species=" + pet.species
        },
        (code:number, response:string) => {
            callback(JSON.parse(response));
        }
    );
}

export function fetchPets(callback:any) {
    ajax.ajax({
            url: "/api/pets",
            method: "GET"
        },
        (code:number, response:string) => {
            callback(JSON.parse(response));
        }
    );
}
