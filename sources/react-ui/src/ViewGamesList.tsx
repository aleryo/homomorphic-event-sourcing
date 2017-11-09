import React from "react";
import * as R from "ramda";
import {GameDescription, Props} from "./types";
import {createGame, joinGame, setNumPlayers, setNumRobots} from "./actions";

export default ({model, dispatch}: Props) => {
    const displayGames = R.partial(displayPossibleGames, [dispatch]);
    switch(model.game.gameType){
        case "SelectGame":
        return (
            <div id="games-list">
                <CreateGame model={model} dispatch={dispatch} />
                <ul>
                    { model.game.games.map(displayGames) }
                </ul>
                }
            </div>
        );
    }
    return null;
}

const CreateGame = ({model, dispatch} : Props) => {
    switch (model.game.gameType) {
        case "SelectGame":
            return <div id="create-game">
                <input type="number" onInput={e => dispatch(setNumPlayers(e.currentTarget.value))} min="0" max="6" value={model.game.numPlayers} />
                <input type="number" onInput={e => dispatch(setNumRobots(e.currentTarget.value))} min="0" max="6" value={model.game.numRobots} />
                <button onClick={() => dispatch(createGame())}></button>
            </div>
    }
    return null;
};

const displayPossibleGames = (dispatch: any, desc : GameDescription) => {
    let isLive = desc.descLive ? "live" : "";

    return (
        <li>
            <div className={"game-description " + isLive}>
                <span className="game-id">{desc.gameDescId}</span>
                <span className="numPlayers humans">{desc.descNumberOfHumans}</span>
                <span className="numPlayers robots">{desc.descNumberOfRobots}</span>
                <ul className="players-list">
                    {desc.descRegisteredHumans.map(name => <li>{name}</li>)}
                </ul>
                {desc.descLive ? null : <button onClick={e => dispatch(joinGame(desc.gameDescId))}>Join</button>}
            </div>
        </li>
    );
};


/*

displayPlayer : PlayerName -> Html Msg
displayPlayer p =
    li [] [ text p ]

/*
viewGamesList : Model -> Html Msg
viewGamesList model =
case model.game of
SelectGame sg ->
    div [ id "games-list" ]
[ createGame model
    , ul [] (List.map displayPossibleGames sg.games)
]

_ ->
    text ""


createGame : Model -> Html Msg
createGame model =
case model.game of
SelectGame sg ->
    div [ id "create-game" ]
[ input [ id "num-players", type_ "number", onInput SetNumPlayers, A.min "0", A.max "6", value (toString sg.numPlayers) ] []
    , input [ id "num-robots", type_ "number", onInput SetNumRobots, A.min "0", A.max "6", value (toString sg.numRobots) ] []
    , button [ onClick CreateGame ] [ text "New GameState" ]
]

_ ->
    text ""


displayPossibleGames : GameDescription -> Html Msg
displayPossibleGames desc =
    let
isLive =
if desc.descLive then
"live"
else
""
in
li []
    [ div [ class <| "game-description " ++ isLive ]
                [ span [ class "game-id" ] [ text desc.gameDescId ]
                , span [ class "numPlayers humans" ] [ text <| toString desc.descNumberOfHumans ]
                , span [ class "numPlayers robots" ] [ text <| toString desc.descNumberOfRobots ]
                , ul [ class "players-list" ]
                    (List.map displayPlayer desc.descRegisteredHumans)
                , if desc.descLive then
                    text ""
                  else
                    button [ onClick <| Join desc.gameDescId ] [ text "Join" ]
                ]
            ]


displayPlayer : PlayerName -> Html Msg
displayPlayer p =
    li [] [ text p ]

*/
