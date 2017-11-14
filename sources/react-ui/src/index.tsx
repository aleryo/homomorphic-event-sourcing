import React from "react";
import ReactDOM from "react-dom";
import {Provider} from "react-redux";
import {createStore, compose, applyMiddleware} from "redux";
import logger from "redux-logger";

import reducers from "./reducers";

import View from "./View";


const store = createStore(reducers,
    compose(
        applyMiddleware(logger)
    )
);

ReactDOM.render(
    <Provider store={store}>
        <View />
    </Provider>
  , document.getElementById("start")
);

