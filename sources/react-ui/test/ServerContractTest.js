import expect from "must";
import WebSocket from 'ws';

describe('Server API', function () {

  // see Acquire.Net.Types: Command and Result

  it('NewGame yields NewGameStarted', function (done) {

    const socket = new WebSocket('ws://localhost:9000/TEST');
    socket.on('open', function open() {
      socket.send(JSON.stringify({tag: "NewGame", numHumans: 1, numRobots: 2}));
    });

    socket.on('message', function incoming(data) {
      console.log(data);
      const payload = JSON.parse(data);
      expect(payload.tag).to.eql("NewGameStarted");
      expect(typeof payload.contents).to.eql("string");
      expect(payload.contents.length).to.be(8);
      done();
    });
  });

  it('List yields GamesList', function (done) {

    const socket = new WebSocket('ws://localhost:9000/TEST');
    socket.on('open', function open() {
      socket.send(JSON.stringify({tag: "List", contents: []}));
    });

    socket.on('message', function incoming(data) {
      console.log(data);
      const payload = JSON.parse(data);
      expect(payload.tag).to.eql("GamesList");
      // assert some more on the payload
      done();
    });
  });

  // how to write a JoinGame test for an existing game?
  it('JoinGame for a non-existing game yields ErrorMessage', function (done) {

    const socket = new WebSocket('ws://localhost:9000/TEST');
    socket.on('open', function open() {
      socket.send(JSON.stringify({tag: "JoinGame", playerName: 'Peter', gameId: 'ABCDEFGH' }));
    });

    socket.on('message', function incoming(data) {
      console.log(data);
      const payload = JSON.parse(data);
      expect(payload.tag).to.eql("ErrorMessage");
      done();
    });
  });

  it('Action yields ', function (done) {

    const socket = new WebSocket('ws://localhost:9000/TEST');
    socket.on('open', function open() {
      socket.send(JSON.stringify({tag: "Action", selectedPlay: 1}));
    });

    // fails because we don't get an answer
    socket.on('message', function incoming(data) {
      console.log(data);
      const payload = JSON.parse(data);
      expect(payload.tag).to.eql("");
      done();
    });
  });

});
