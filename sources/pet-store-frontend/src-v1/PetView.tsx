import React from 'react';
import {Pet} from './types';


const pictures : { [index:string] : string } = {
    "Cat": require('../img/cat.jpg'),
    "Dog": require('../img/dog.jpg'),
    "Canary": require('../img/canary.jpg'),
    "Rabbit": require('../img/rabbit.jpg'),
    "Fish": require('../img/fish.jpg')
};


export default ({name, species}: Pet) => (
    <div>
        <img height="20" src={pictures[species]}/><span>{name}</span><br/>
    </div>
);
