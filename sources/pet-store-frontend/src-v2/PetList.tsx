import React from 'react';
import {Pet} from './types';
import PetView from './PetView';
import PetSale from './PetSale';


export default ({pets, sellPet}: { pets: Pet[], sellPet: any }) => (
    !pets || !pets.length ? null :
        <div>
            <h3>Pets in our Store</h3>
            <ul>
                {pets.map((pet: Pet) => <span key={pet.name + '#' + pet.species}>
                    <PetView {...pet} /><PetSale pet={pet} sellPet={sellPet}/>
                </span>)}
            </ul>
        </div>
);
