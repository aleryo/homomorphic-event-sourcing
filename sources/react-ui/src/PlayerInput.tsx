import React from "react";
import {Props} from "./types";
import {registerPlayer, setName} from "./actions";



export default ({model: {game}, dispatch}: Props) => {
    switch(game.type){
        case "Register": return <div id="player-id">
            <span>Player Name</span>
            <input id="player-name" value={game.player.playerName} onChange={e => dispatch(setName(e.currentTarget.value))}/>
            <button onClick={() => { dispatch(registerPlayer()); dispatch({ type: 'List' }); } }>Register</button>
        </div>
    }
    return null;
}


/*
playerInput : Model -> Html Msg
playerInput model =
case model.game of
Register p ->
    div [ id "player-id" ]
[ span [] [ text "Player Name" ]
    , input [ id "player-name", value p.player.playerName, onInput SetName ] []
    , button [ onClick RegisterPlayer ] [ text "Register" ]
]

_ ->
    text ""
*/
