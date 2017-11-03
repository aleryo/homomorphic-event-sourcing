import React from "react";
import {GameState, Props} from "./types";


function subtitleFor(game: GameState) {
    switch(game.gameType) {
        case "PlayGame":
        case "EndOfGame":
            return game.player.playerName + "@" + game.gameId;
    }
    return game.player.playerName;
}


export default ({model: {game}}: Props) => <div id="game-title">
    <h1>Acquire</h1>
    <h2>{subtitleFor(game)}</h2>
</div>


/*

viewTitle : Model -> Html Msg
viewTitle model =
    div [ id "game-title" ]
[ h1 [] [ text "Acquire" ]
    , case model.game of
Register { player } ->
h2 [] [ text player.playerName ]

SelectGame { player } ->
h2 [] [ text player.playerName ]

PlayGame { player, gameId } ->
h2 [] [ text player.playerName, text "@", text gameId ]

EndOfGame { player, gameId } ->
h2 [] [ text player.playerName, text "@", text gameId ]
]

*/
