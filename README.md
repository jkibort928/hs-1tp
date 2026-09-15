# hs-1tp

TODO: HTTPS support

A simple haskell program that presents a single file for a one-time transfer over self-signed HTTPS, located by a unique ID. 

Will only respond to HEAD and GET requests targeted at the generated ID.
Upon any matching GET request, the program will terminate, whether succesful or not.

(adapted code from [hs-ttp](https://github.com/jkibort928/hs-ttp))

# INSTALLATION

- [Install ghc and cabal using ghcup](https://www.haskell.org/ghcup/) if you haven't already
- Run `cabal install` in the repo directory to install the binary into ~/.cabal/bin

# USAGE
```
  USAGE:
    hs-1tp [OPTIONS] <FILE>

  OPTIONS:
    -h, --help        Display this help message
    --version         Display the server version
    -p, --port        Specify a port (default: 40443)

  DESCRIPTION:
    Starts a basic, non-concurrent HTTPS server that exposes a single
    file for a one-time transfer. A randomly generated ID is printed
    in a message to stdout upon startup.

    The server only responds to GET or HEAD requests matching the
    exact URI of the generated ID. The server terminates upon a
    properly targeted GET request, regardless if it was successful.
```
