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

export default class extends Component<Props, Pet> {

    constructor(props: Props) {
        super(props);
        this.state = { name: "Pet", species: "" };
    }

    render() {
        return (
            <div>
                <h3>Add a New Pet</h3>
                <div>
                    <label>Pet name: </label>
                    <input type="text"
                           onBlur={e => this.setState({name: e.currentTarget.value})}/>
                </div>
                <div>
                    <label>Species: </label>
                    <button onClick={() => this.setState({species: "cat"})} style={buttonStyle}><img height="50" src={cat}/></button>
                    <button onClick={() => this.setState({species: "dog"})} style={buttonStyle}><img height="50" src={dog}/></button>
                    <button onClick={() => this.setState({species: "canary"})} style={buttonStyle}><img height="50" src={canary}/></button>
                    <button onClick={() => this.setState({species: "rabbit"})} style={buttonStyle}><img height="50" src={rabbit}/></button>
                    <button onClick={() => this.setState({species: "fish"})} style={buttonStyle}><img height="50" src={fish}/></button>
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
