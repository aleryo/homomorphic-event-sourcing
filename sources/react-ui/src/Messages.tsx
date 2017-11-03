import React from "react";
import {Props} from "./types";
import {displayMessages, hideMessages} from "./actions";

export default ({model: {showMessages, strings}, dispatch}: Props) => {
    const toggle = showMessages
        ? <span className="fa fa-toggle-down" onClick={() => dispatch(hideMessages())}/>
        : <span className="fa fa-toggle-right" onClick={() => dispatch(displayMessages())}/>;
    const messages = showMessages
        ? <div id="messages-content" style={{height: "10em"}}>
            <ul>
                {strings.map(str => <ShowMessage text={str}/>)}
            </ul>
        </div>
        : null;

    return <div id="messages">
        <div id="messages-header">
            <h1>Messages</h1>
            {toggle}
        </div>
        {messages}
    </div>
}

const ShowMessage = ({text} : {text: string}) => <li className="message">{text}</li>


/*
messages : Model -> Html Msg
messages model =
    let
toggle =
if model.showMessages then
span [ class "fa fa-toggle-down", onClick <| ShowMessages False ] []
else
span [ class "fa fa-toggle-right", onClick <| ShowMessages True ] []

doDisplayList =
if model.showMessages then
    [ A.style [ ( "height", "10em" ) ] ]
else
    [ A.style [ ( "height", "0" ) ] ]
    in
    div [ id "messages" ]
[ div [ id "messages-header" ]
    [ h1 [] [ text "Messages" ]
, toggle
]
, div ([ id "messages-content" ] ++ doDisplayList)
[ ul [] <| List.map showMessage model.strings
]
]

showMessage : String -> Html Msg
showMessage s =
    li [ class "message" ] [ text s ]

*/
