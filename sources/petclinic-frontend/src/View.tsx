import React, {Component} from 'react';

import {Model, Props} from './types';

import {connect} from 'react-redux';
import PetAdmission from './PetAdmission';

export class ViewComponent extends Component<Props> {

    componentWillMount() {
        // TODO load data from backend
    }

    render() {
        const {model, dispatch} = this.props;
        return <div>
            Hallo Hallo
            <PetAdmission admitPet={()=>{}} />

        </div>;
    }
}

export default connect((state: Model) => state)(ViewComponent);
