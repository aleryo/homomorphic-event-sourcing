# ...

## React.js Frontend

### Installation Instructions

1. `cd react-ui`
1. `npm install` -- to download all required 3rd party modules

#### Running the Frontend from Webpack's Dev Server

1. `npm start` (on Windows: `npm start-win`) -- launches the webpack-dev-server
1. Go to [localhost:3000](http://localhost:3000) to access the frontend

## Haskell Backend

### Installation Instructions

This assumes you have [stack](http://docs.haskellstack.org) installed on your system. Please follow instructions on the website for how to install stack.

Build the code:

```
$ stack setup
# might take a little while...
# ...
$ stack test
```

### Running the server

To start an acquire HTTP server on port 9000:

```
$ stack exec server -- 9000 react-ui
```

**TBD**: Then point browser at `http://localhost:9000/index.html` to load the UI

### Running mock server

To run a mock websocket server on port 9000 against which to test a client:

```
$ stack exec wsmock -- 9000
```

**WORK IN PROGRESS**: This server implements an `IOAutomaton` based protocol.

### Testing mock WS server

There is a very simple client program that can be used to test the mock WS server:

```
$ stack exec wsclient -- 127.0.0.1 9000 foo
Connected!
{"tag": "List"}
{"reason":"command List not handled"}
{"tag":"Bye"}
wsclient: CloseRequest 1000 "Bye"
^C
$
```
