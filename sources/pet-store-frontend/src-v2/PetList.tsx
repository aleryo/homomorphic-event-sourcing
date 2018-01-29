import React from 'react';
import {Pet} from './types';
import PetView from './PetView';
import PetSale from './PetSale';


export default ({pets, sellPet, petSaleOK}: { pets: Pet[], sellPet: any, petSaleOK: boolean }) => (
    !pets || !pets.length ? null :
        <div>
            <h3>Pets in our Store</h3>
            <ul>
                {pets.map((pet: Pet) => <span key={pet.name + '#' + pet.species}>
                    <PetView {...pet} />{petSaleOK ? <PetSale pet={pet} sellPet={sellPet}/> : null}
                </span>)}
            </ul>
        </div>
);
