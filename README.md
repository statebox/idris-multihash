# Idris-multihash

Multihash implementation for Idris. It depends on the [idris-bytes](https://github.com/ziman/idris-bytes) library

## How to install

Clone the project

```
$ git clone git@github.com:statebox/idris-multihash.git
```

Install with

```
$ idris --install multihash.ipkg
```

## How to use

Import in your project with

```
import Data.Multihash
```

and tell idris to use the package with

```
idris -p idris-multihash
```

The `Data.Multihash` import gives you access to the Multihash error type and to `decode` and
`encode` functions which manipulate `Bytes`

## Bytes dependency

Since Multihash depends on bytes you might want to use it as well. Head to the [idris-bytes](https://github.com/ziman/idris-bytes) library in order to get more info. To summarize:

- Clone the idris-bytes project
- Install it
- Add `import Data.Bytes` (and maybe `import Data.ByteArray`) at the top of your file
- run idris with `-p bytes` (typically `idris -p idris-multihash -p bytes`)
