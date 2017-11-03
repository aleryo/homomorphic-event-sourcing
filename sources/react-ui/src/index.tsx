import React from "react";
import ReactDOM from "react-dom";
import View from "./View";
import {INITIAL_STATE} from "./reducers";

ReactDOM.render(
    <View model={INITIAL_STATE} />
  , document.getElementById("start")
);

