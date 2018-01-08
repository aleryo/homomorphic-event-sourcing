import React from 'react';
import {Pet} from './types';
import PetView from './PetView';


export default ({pets}: { pets: Pet[] }) => (
    !pets || !pets.length ? null :
        <div>
            <h3>Pets in the Clinic</h3>
            <ul>
                {pets.map((pet: Pet) => <PetView {...pet} />)}
            </ul>
        </div>
);
