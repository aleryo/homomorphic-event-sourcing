import React, {Component} from 'react';

import {Model, Props, Pet} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAdmission';
import {admitPet} from './reducers';
import PetList from './PetList';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        // TODO load data from backend
    }

    render() {
        return <div>
            <h2>Welcome to our Pet Clinic</h2>
            <PetAdmission admitPet={(pet:Pet)=> this.props.dispatch(admitPet(pet))} />
            <PetList pets={this.props.admittedPets} />
        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
