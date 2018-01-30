import React, {Component} from 'react';

import {Model, Props, Pet} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAddition';
import {addAndSubmitPet, loadPets, sellAndSubmitPet} from './actions';
import PetList from './PetList';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        this.props.dispatch(loadPets());
    }

    render() {
        return <div>
            <h2>Welcome to our Pet Store</h2>
            <PetAdmission admitPet={(pet:Pet) => this.props.dispatch(addAndSubmitPet(pet))} />
            <PetList pets={this.props.pets} sellPet={(pet:Pet) => this.props.dispatch(sellAndSubmitPet(pet))} />
        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
