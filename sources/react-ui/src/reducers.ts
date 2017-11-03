
import {Model} from "./types";

export const INITIAL_STATE : Model = {
    game: {gameType: "Register", player: {ownedCash: 0, playerType: "human", playerName: "Fred"}},
    showMessages: true,
    errors: ["An error!", "And another!"],
    strings: ["A string!", "And another!"]
};
