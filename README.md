# hs-1tp

A simple haskell program that presents a single file for a one-time transfer over self-signed HTTPS, located by a unique ID. 

Will only respond to HEAD and GET requests targeted at the generated ID.
Upon any matching GET request, the program will terminate, whether succesful or not.

# INSTALLATION

- [Install ghc and cabal using ghcup](https://www.haskell.org/ghcup/) if you haven't already
- Run `cabal install` in the repo directory to install the binary into ~/.cabal/bin

# USAGE

    hs-1tp [OPTIONS] <FILE>

    [OPTIONS]: 
        -h:
        --help:             Display this help message

        --version:          Display the server version

        -p:
        --port:             Specify a port

    <FILE>:
        The target file for a one-time transfer.

    This program will start a basic, non-concurrent HTTPS server that exposes
    	a single file for a one-time transfer.
    This program will send a randomly generated ID to stdout upon startup.
    
    The server will only respond to requests with the URI exactly as "/<GENERATED_ID>",
    	where <GENERATED_ID> is the aforementioned initial ID from stdout.
    The server will only respond to HEAD or GET requests.
    The server will terminate on a GET request (using the correct URI),
    	regardless if it was successful or not.
    
    The server binds to the wildcard address, meaning it will be accessible on any ip interface.
