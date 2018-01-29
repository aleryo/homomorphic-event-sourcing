import React, {Component} from 'react';
import {Pet} from './types';

const cat = require('../img/cat.jpg');
const dog = require('../img/dog.jpg');
const canary = require('../img/canary.jpg');
const rabbit = require('../img/rabbit.jpg');
const fish = require('../img/fish.jpg');

const buttonStyle = {
    margin: "3px"
};

interface Props {
    admitPet: any
}

const labelStyle = {
    display: "block",
    width: "75px"
};

export default class extends Component<Props, Pet> {

    constructor(props: Props) {
        super(props);
        this.state = { name: "Pet", species: "", price: 0 };
    }

    render() {
        return (
            <div>
                <h3>Add a New Pet</h3>
                <div>
                    <label style={labelStyle}>Pet name: </label>
                    <input type="text"
                           onBlur={e => this.setState({name: e.currentTarget.value})}/>
                </div>
                <div>
                    <label style={labelStyle}>Pet price: </label>
                    <input type="text"
                           onBlur={e => this.setState({price: parseInt(e.currentTarget.value, 10)})}/>
                </div>
                <div>
                    <label style={labelStyle}>Species: </label>
                    <button onClick={() => this.setState({species: "Cat"})} style={buttonStyle}><img height="50" src={cat}/></button>
                    <button onClick={() => this.setState({species: "Dog"})} style={buttonStyle}><img height="50" src={dog}/></button>
                    <button onClick={() => this.setState({species: "Canary"})} style={buttonStyle}><img height="50" src={canary}/></button>
                    <button onClick={() => this.setState({species: "Rabbit"})} style={buttonStyle}><img height="50" src={rabbit}/></button>
                    <button onClick={() => this.setState({species: "Fish"})} style={buttonStyle}><img height="50" src={fish}/></button>
                </div>
                <div>
                    <button
                        onClick={() => {
                            this.props.admitPet(this.state);
                        }}
                    >Add {this.state.name}{this.state.species ? ", a" : ""} {this.state.species}
                    </button>
                </div>
            </div>
        );
    }
}
