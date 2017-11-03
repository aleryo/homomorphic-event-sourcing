import React from "react";

import ViewTitle from "./ViewTitle";
import DisplayErrors from "./DisplayErrors";
import Messages from "./Messages";
import PlayerInput from "./PlayerInput";
import ViewGameResult from "./ViewGameResult";
import {Props} from "./types";
import ViewGamesList from './ViewGamesList';

/*
view : Model -> Html Msg
view model =
    div []
        [ viewTitle model
    , displayErrors model
    , playerInput model
    , viewGamesList model
    , gameBoard model
    , viewGameResult model
    , messages model
]
 */



export default ({model} : Props) => <div>
    <ViewTitle model={model} />
    <DisplayErrors model={model} />
    <PlayerInput model={model} />
    <ViewGamesList model={model} />
    {
        /*
    <GameBoard model={model} />
        */
    }
    <ViewGameResult model={model} />
    <Messages model={model} />
</div>


/*
module View exposing (view)

-- {{{

    import Dict
    import Json.Decode as Json
    import Html exposing (..)
    import Html.Attributes exposing (href, src, placeholder, min, max, value, id, class, type_)
    import Html.Attributes as A
    import Html.Events exposing (..)
    import Messages exposing (..)
    import Model exposing (..)
    import String


    -- }}}
-- {{{


    gameBoard : Model -> Html Msg
    gameBoard model =
case model.game of
    PlayGame g ->
        div [ id "game-board" ]
    [ div [ class "player" ]
        [ h1 [] [ text "Player's Hand" ]
, span [ class "cash" ] [ text <| toString g.player.ownedCash ]
        , div [ class "stock" ] <| List.map displayStock (Dict.toList g.player.ownedStock)
]
, div [ class "plays" ]
    (h1 [] [ text "Possible Plays" ] :: (List.indexedMap displayPlay g.possiblePlays))
, div [ class "board" ]
    (h1 [] [ text "Current Board" ] :: List.map (displayCell g.highlightedCell) (Dict.toList g.board))
]

    _ ->
        text ""


    displayStock : ( ChainName, Int ) -> Html Msg
    displayStock ( cn, num ) =
        span [ class cn ]
            [ span [ class "stock-count" ] [ text <| toString num ]
]


    displayPlay : Int -> Messages.Order -> Html Msg
    displayPlay n order =
case order of
    Place _ ( r, c ) ->
    span
        [ class "cell empty"
        , onClick <| Play (n + 1)
        , onMouseEnter (HighlightCell ( r, c ))
        , onMouseLeave UnhighlightCell
]
    [ span [ class "cell-content" ]
        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ] ]
]

    BuyStock _ cn ->
        span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-usd" ] [] ]
                ]

        SellStock _ cn num price ->
            span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-usd" ] [] ]
                ]

        ExchangeStock _ cf ct count ->
            span [ class <| "cell exchange", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class <| "buyee-" ++ cf ] []
                    , span [ class <| "buyer-" ++ ct ] []
                    , span [ class "fa fa-lg fa-exchange" ] []
                    ]
                ]

        Fund _ cn _ ->
            span [ class <| "cell chain " ++ cn, onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-building-o" ] [] ]
                ]

        Merge _ _ cf ct ->
            span [ class <| "cell merge", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class <| "buyee-" ++ cf ] []
                    , span [ class <| "buyer-" ++ ct ] []
                    , span [ class "fa fa-lg fa-building-o" ] []
                    ]
                ]

        Pass ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-refresh" ] [] ]
                ]

        EndGame ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-stop" ] [] ]
                ]

        Cancel ->
            span [ class <| "cell", onClick <| Play (n + 1) ]
                [ span [ class "cell-content" ]
                    [ span [ class "fa fa-lg fa-backward" ] [] ]
                ]


displayCell : Maybe Tile -> ( Tile, Cell ) -> Html Msg
displayCell highlighted ( ( r, c ) as tile, cell ) =
    let
        hlClass =
            maybe " empty"
                (\hlTile ->
                    if hlTile == tile then
                        " highlighted"
                    else
                        " empty"
                )
                highlighted
    in
        case cell.cellContent of
            Empty ->
                span [ class <| "cell" ++ hlClass ]
                    [ span [ class "cell-content" ]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ] ]
]

Neutral _ ->
span [ class <| "cell neutral" ]
                    [ span [ class "cell-content" ]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ] ]
]

Chain n ->
span [ class <| "cell chain " ++ n ]
                    [ span [ class "cell-content" ]
                        [ span [] [ text <| String.fromChar r ++ "-" ++ toString c ] ]
]

_ ->
text ""




-- * Utilities


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown" (Json.map (always msg) keyDecoder)


keyDecoder : Json.Decoder (Result String ())
keyDecoder =
    let
        convert : Int -> Json.Decoder (Result String ())
        convert code =
            if code == 13 then
                Json.succeed (Ok ())
            else
                Json.fail ("not the right key code")
    in
        Json.int |> Json.andThen convert


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()
    else
        Err "not the right key code"





-- | Optional mapping of some `Maybe a` value
-- This is from Haskell's Data.Maybe package. Given a `Maybe a`, either returns
-- some `defaultValue` if `optional` is `Nothing` or applies given `transform` function
-- on the content of `Just a` value.


maybe : b -> (a -> b) -> Maybe a -> b
maybe defaultVal transform optional =
Maybe.withDefault defaultVal <|
Maybe.map transform optional



-- }}}

    */
