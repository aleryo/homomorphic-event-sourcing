import React, {Component} from 'react';

import {Model, Props, Pet} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAddition';
import {addAndSubmitPet, loadPets, loginAndSubmitUser, loginUser, sellPet} from './actions';
import PetList from './PetList';
import UserLogin from './UserLogin';
import {isNullOrUndefined} from 'util';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        this.props.dispatch(loadPets());
    }

    render() {
        return <div>
            <h2>Welcome to our Pet Store</h2>
            <PetAdmission admitPet={(pet:Pet) => this.props.dispatch(addAndSubmitPet(pet))} />
            <PetList pets={this.props.pets} sellPet={(pet:Pet) => this.props.dispatch(sellPet(pet))} petSaleOK={this.props.user !== null} />
            <UserLogin user={this.props.user} loginUser={(name:string) => this.props.dispatch(loginAndSubmitUser(name))} />
        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
