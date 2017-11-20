import actions from './actions'
import {Cell, ChainName, SimpleMap, Tile} from './types';

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
        store.dispatch({type: 'ConnectWS', url: "ws://localhost:9000/FOO"});
    };

    const onMessage = (ws:any,store:any) => (evt:any) => {
        //Parse the JSON message received on the websocket
        const msg = JSON.parse(evt.data);
        console.log("Received message ", msg);
        switch(msg.tag) {
            case "GamesList":
                //Dispatch an action that adds the received message to our state
                store.dispatch({ type: 'GamesList', games: msg.contents });
                break;
            case 'NewGameStarted':
                store.dispatch({ type: 'NewGameStarted', gameId: msg.contents });
                break;
            case 'GameStarts':
                store.dispatch({ type: 'GameStarts', gameId: msg.contents });
                break;
            case 'GameState':
                // FIXME mutation!
                msg.gsPlayer.ownedStock = new SimpleMap<ChainName, number>(msg.gsPlayer.ownedStock.stock);
                store.dispatch({ type: 'GameUpdated', board: new SimpleMap<Tile, Cell>(msg.gsBoard), playables: msg.gsPlayables, player: msg.gsPlayer });
                break;
            default:
                console.log("Received unknown message type: '" + msg.tag + "'");
                break;
        }
    };

    return (store:any) => (next:any) => (action:any) => {
        switch(action.type) {

            //The user wants us to connect
            case 'ConnectWS':
                console.log("CONNECT")
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
                socket.onopen = onOpen(socket,store,action.token);

                break;

            //The user wants us to disconnect
            case 'DISCONNECT':
                console.log("DISCONNECT")
                if(socket != null) {
                    socket.close();
                }
                socket = null;

                //Set our state to disconnected
                // store.dispatch(actions.disconnected());
                break;

            //Send actions down the websocket to the server
            case 'RegisterPlayer':
            case 'NewGameStarted':
            case 'Reset':
                console.log("RegisterPlayer or Reset")
                socket.send(JSON.stringify({tag: "List"}));
                break;

            case 'CreateGame': {
                console.log("CreateGame")
                const {game} = store.getState();
                // caution: only works in SelectGame state...
                socket.send(JSON.stringify({tag: "NewGame", numHumans: game.numPlayers, numRobots: game.numRobots}));
                break;
            }

            case 'Join': {
                console.log("Join")
                const {game} = store.getState();
                // caution: only works in SelectGame state...
                socket.send(JSON.stringify({tag: "JoinGame", playerName: game.player.playerName, gameId: action.gameDescId }));
                break;
            }
        }
        // pass the actions to the next handler (we need to send them to the redux store)
        return next(action);
    }

})();

export default socketMiddleware;
