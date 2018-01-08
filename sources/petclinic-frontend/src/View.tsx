import React, {Component} from 'react';

import {Model, Props, Pet} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAdmission';
import {admitPet} from './reducers';
import PetView from './PetView';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        // TODO load data from backend
    }

    render() {
        return <div>
            Hallo Hallo
            <PetAdmission admitPet={(pet:Pet)=> this.props.dispatch(admitPet(pet))} />
            <PetView {...this.props.admittedPets[0]} />
        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
