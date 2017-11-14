//import {Map} from "core-js";

import React from 'react';
import * as R from 'ramda';
import {Player, Players, Props} from './types';
import {reset} from './actions';


export default ({model: {game}, dispatch}: Props) => {
    switch (game.type) {
        case 'EndOfGame':
            return displayPlayerResults(game.gameResult, dispatch);
    }
    return null;
}

const displayPlayerResults = (players: Players, dispatch: any) => {
    const winners = R.sortBy(R.prop('ownedCash'), players.values());
    return <div id="game-result-background">
        <div id="game-result">
            <h1>Players' Score</h1>
            <button onClick={() => dispatch(reset())}>Reset</button>
            {winners.map((winner: Player) => <DisplayPlayerResult player={winner}/>)}
        </div>
    </div>;
};

const DisplayPlayerResult = ({player}: { player: Player }) => (
    <div className={'player ' + player.playerType}>
        <span className="name">{player.playerName}</span>
        <span className="cash">{player.ownedCash}</span>
    </div>
);

/*
viewGameResult : Model -> Html Msg
viewGameResult model =
case model.game of
EndOfGame g ->
    displayPlayerResults (Dict.values g.gameResult)

_ ->
    div [ id "game-result-background", A.style [ ( "display", "none" ) ] ] []


displayPlayerResults : List Player -> Html Msg
displayPlayerResults players =
    let
winners =
    List.sortBy (\p -> p.ownedCash) players
in
div [ id "game-result-background" ]
[ div [ id "game-result" ]
(h1 [] [ text "Players' Score" ]
:: button [ onClick Reset ] [ text "Reset" ]
    :: List.map displayPlayerResult winners
)
]


displayPlayerResult : Player -> Html Msg
displayPlayerResult player =
    div [ class <| "player " ++ Messages.showPlayerType player.playerType ]
        [ span [ class "name" ] [ text player.playerName ]
        , span [ class "cash" ] [ text <| toString player.ownedCash ]
        ]

*/
