import React from 'react';
import {Pet} from './types';


export default ({pet, sellPet}: {pet: Pet, sellPet: any}) => (
    <button onClick={() => sellPet(pet)}>Buy {pet.name}</button>
);
