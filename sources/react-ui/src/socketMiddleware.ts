import actions, {Action} from './actions';
import {Cell, ChainName, Player, SimpleMap, Tile} from './types';

const decodeOrder = ({tag, contents}:{tag:string, contents:any[]}) => {
    switch(tag){
        case 'Place':
            return { tag: 'Place', playerName: contents[0], tile: contents[1]};
        case 'Merge':
            return { tag: 'Merge', playerName: contents[0], tile: contents[1], fromChain: contents[2], toChain: contents[3] };
        case 'Fund':
            return { tag: 'Fund', playerName: contents[0], chainName: contents[1], tile: contents[2] };
        case 'BuyStock':
            return { tag: 'BuyStock', playerName: contents[0], chainName: contents[1] };
        case 'SellStock':
            return { tag: 'SellStock', playerName: contents[0], chainName: contents[1], amount: contents[2], todo: contents[3] }; // FIXME param name
        case 'ExchangeStock':
            return { tag: 'ExchangeStock', playerName: contents[0], fromChain: contents[1], toChain: contents[2], amount: contents[3] };
        case 'Pass':
            return { tag: 'Pass' };
        case 'EndGame':
            return { tag: 'EndGame' };
        case 'Cancel':
            return { tag: 'Cancel' };
        default:
            console.log("Unknown move tag " + tag + " with ", contents);
    }
};

const decodePlayer = ({playerName, playerType, tiles, ownedStock, ownedCash}: any) =>
    ({playerName, playerType, tiles, ownedStock: new SimpleMap<ChainName, number>(ownedStock.stock), ownedCash});

const decodeBoard = (board: any) => new SimpleMap<Tile, Cell>(board);

const decodeGame = ({gameId, gameBoard, players}: any) =>
    ({gameId, gameBoard: decodeBoard(gameBoard), players: players.map(decodePlayer)});

const socketMiddleware = (function(){
    let socket:any = null;

    const onOpen = (ws:any,store:any,token:any) => (evt:any) => {
        //Send a handshake, or authenticate with remote end

        //Tell the store we're connected
        console.log("connected");
        //store.dispatch(actions.connected());
    };

    const onClose = (ws:any,store:any) => (evt:any) => {
        //Tell the store we've disconnected
        console.log("disconnected");
        //store.dispatch(actions.disconnected());
        // reconnect again:
        store.dispatch({type: 'ConnectWS', url: "ws://localhost:9000/FOO"});
    };

    const onMessage = (ws:any,store:any) => (evt:any) => {
        //Parse the JSON message received on the websocket
        const msg = JSON.parse(evt.data);
        switch(msg.tag) {
            case "PlayerRegistered":
                // TODO does backend actually send this?
                store.dispatch({ type: "PlayerRegistered", playerName: msg.contents[0], gameId: msg.contents[1] });
                break;
            case "GamesListed":
                //Dispatch an action that adds the received message to our state
                store.dispatch({ type: 'GamesListed', games: msg.contents });
                break;
            case 'NewGameCreated':
                store.dispatch({ type: 'NewGameCreated', gameId: msg.contents });
                break;
            case 'GameStarted':
                store.dispatch({ type: 'GameStarted', gameId: msg.contents });
                break;
            case 'GameState':
                console.log("GameState", msg)
                store.dispatch({ type: 'GameUpdated', board: decodeBoard(msg.gsBoard), possibleMoves: msg.gsPlayables.map(decodeOrder), player: decodePlayer(msg.gsPlayer) });
                break;
            case "Played":
                store.dispatch({ type: 'Played', playerName: msg.gsPlayerName, board: decodeBoard(msg.gsBoard), played: decodeOrder(msg.gsPlayed) });
                break;
            case "GameEnds":
                // FIXME decodeGame bails out here because players is not a list
                store.dispatch({ type: 'GameEnds', endGame: decodeGame(msg.gsEndGame) });
                break;
            case "ErrorMessage":
                store.dispatch({ type: 'ErrorMessage', message: msg.contents[0] });
                break;
            default:
                console.log("Received unknown message type: '" + msg.tag + "'");
                break;
        }
    };

    return (store:any) => (next:any) => (action:Action) => {
        switch(action.type) {

            //The user wants us to connect
            case 'ConnectWS':
                //Start a new connection to the server
                if(socket != null) {
                    socket.close();
                }
                //Send an action that shows a "connecting..." status for now
                // store.dispatch(actions.connecting());

                //Attempt to connect (we could send a 'failed' action on error)
                socket = new WebSocket(action.url);
                socket.onmessage = onMessage(socket,store);
                socket.onclose = onClose(socket,store);
                socket.onopen = onOpen(socket,store,action.type);

                break;

            //The user wants us to disconnect
                /*
            case 'DISCONNECT':
                if(socket != null) {
                    socket.close();
                }
                socket = null;

                //Set our state to disconnected
                // store.dispatch(actions.disconnected());
                break;
                */

            //Send actions down the websocket to the server
            case 'RegisterPlayer':
            case 'PlayerRegistered':
            case 'NewGameCreated':
            case 'Reset':
                socket.send(JSON.stringify({tag: "List", contents: []}));
                break;

            case 'CreateGame': {
                const {game} = store.getState();
                // caution: only works in SelectGame state...
                socket.send(JSON.stringify({tag: "CreateGame", numHumans: game.numPlayers, numRobots: game.numRobots}));
                break;
            }

            case 'JoinGame': {
                const {game} = store.getState();
                // caution: only works in SelectGame state...
                socket.send(JSON.stringify({tag: "JoinGame", playerName: game.player.playerName, gameId: action.gameDescId }));
                break;
            }

            case 'Move': {
                socket.send(JSON.stringify({tag: "Action", selectedPlay: action.move}));
                break;
            }
        }
        // pass the actions to the next handler (we need to send them to the redux store)
        return next(action);
    }

})();

export default socketMiddleware;
