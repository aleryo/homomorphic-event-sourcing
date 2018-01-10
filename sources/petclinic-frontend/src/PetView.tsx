import React from 'react';
import {Pet} from './types';


const pictures : { [index:string] : string } = {
    cat: require('../img/cat.jpg'),
    dog: require('../img/dog.jpg'),
    bird: require('../img/canary.jpg'),
    rabbit: require('../img/rabbit.jpg'),
    fish: require('../img/fish.jpg')
};


export default ({name, species}: Pet) => (
    <div>
        <img height="20" src={pictures[species]}/><span>{name}</span><br/>
    </div>
);
