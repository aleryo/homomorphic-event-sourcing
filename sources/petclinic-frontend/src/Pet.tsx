import React from "react";
import {Pet} from './types';

export default ({name, species}:Pet) => (
  <div>
    <label>Pet name: </label><span>{name}</span><br/>
    <label>Species: </label><span>{species}</span><br/>
  </div>
);
