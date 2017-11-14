import React from 'react';
import {Props} from './types';

export default ({model: {errors}}: Props) => (
    <div id="errors">
        {errors.map(error => <div className="error" key={error}>{error}</div>)}
    </div>
)


/*

displayErrors : Model -> Html Msg
displayErrors model =
    div [ id "errors" ]
(List.map displayError model.errors)


displayError : String -> Html Msg
displayError error =
    div [ class "error" ]
[ text error ]

*/
