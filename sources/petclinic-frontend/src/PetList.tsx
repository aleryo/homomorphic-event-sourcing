import React from 'react';
import {Pet} from './types';
import PetView from './PetView';


export default ({pets}: {pets: Pet[]}) => (
    <ul>
        { pets.map((pet:Pet) => <PetView {...pet} />)}
    </ul>
);
