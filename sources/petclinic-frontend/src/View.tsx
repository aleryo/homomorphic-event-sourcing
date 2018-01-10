import React, {Component} from 'react';

import {Model, Props, Pet} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAddition';
import {addPet, sellPet} from './reducers';
import PetList from './PetList';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        // TODO load data from backend
    }

    render() {
        return <div>
            <h2>Welcome to our Pet Store</h2>
            <PetAdmission admitPet={(pet:Pet)=> this.props.dispatch(addPet(pet))} />
            <PetList pets={this.props.pets} sellPet={(pet:Pet) => this.props.dispatch(sellPet(pet))} />
        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
