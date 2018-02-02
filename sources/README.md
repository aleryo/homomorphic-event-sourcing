# Pet Store Demo

## React.js Frontend

**TBD**

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

To start a PetStore HTTP server on port 9000:

```
$ stack exec pet-store-server -- 9000
```

## Model-Based Testing

We provide a complete model for a REST-based PetStore service along with 2 executables for:

1. Test-drive a client by providing a *mock* server
2. Test-drive a server by providing a *driver* generating tests

### Running the Pet Store mock server

```
cd sources/pet-store
stack build
stack exec mock-petstore -- 9090
```

### Running the Pet Store test Driver

To run a test driver that will generate traces and test a server at `localhost:9090`

```
$ stack exec driver-petstore -- localhost 9090
```

### Docker

Both *mock* and *driver* can be run as docker containers, in 2 different versions:

* v1 contains basic store management for adding and removing pets
* v2 adds user basket interaction: User can login, logout, add/remove pets from basket and checkout providing credit card number

To run mock server within a container, exposing port 9090:

```
docker run -rm -p 9090:9090 --name mock-petstore aleryo/pet-store-mock-petstore:v1 9090```
```

To run the driver against the mock server or another server within a container:

```
docker run --link mock-petstore:mock-petstore aleryo/pet-store-driver-petstore:v2 mock-petstore 9090
```

To run the driver against a *local* server, assuming server listens on all local IPs:

```
# check IP for interfaces on the machine, using ifconfig/ipconfig/ip addr
# let's assume this gives 192.168.0.2
docker run aleryo/pet-store-driver-petstore:v2 192.168.0.2 9090
```

### Switching the Frontend between Versions 1 and 2 of the model

In the file `webpack.config.js` around line 10, you will find `"./src-v1/index.tsx",`. Here you can either choose v1 or v2.
Make sure to restart webpack for the change to become active!
