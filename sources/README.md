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

Both *mock* and *driver* can be run as docker containers:

To run mock server within a container, exposing port 9090:

```
docker run --name mock-petstore -p 9090 aleryo/pet-store-mock-petstore:v2 9090
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
