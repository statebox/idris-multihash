# Idris-multihash

[Multihash](https://github.com/multiformats/multihash/) implementation for Idris. 

Using the [idris-bytes](https://github.com/ziman/idris-bytes) library you can encode and decode types using the 
`IMultihash` interface.

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

## API


You can use the `encode` and `decode` functions from the `IMultihash` interface provided you have an instance for your time.

The current implementation support encoding and decoding `Bytes`.

### Encoding 

```idris
||| Encode a digest along with the hash algorithm that was used
decode : hash -> Either MultihashError (Multihash hash)
```

Implement this function in order to decode a `hash` type into a `Multihash`. `MultihashError` can be either `CodeNotFound` or `ParseErrro`.

### Decoding

```idris
||| Attempts to decode raw bytes into a multihash
encode : HashAlgorithm -> hash -> Multihash hash
```

Implement this function in order to encode a `hash` given a supported `HashAlgorithm`.

The supported list of hashes are listed in the definition of `HashAlgorithm`:

```idris
data HashAlgorithm
  = SHA1
  | SHA256
  | SHA512
  | SHA3
  | BLAKE2B
  | BLAKE2S
```

## Bytes dependency

Since Multihash depends on bytes you might want to use it as well. Head to the [idris-bytes](https://github.com/ziman/idris-bytes) library in order to get more info. To summarize:

- Clone the idris-bytes project
- Install it
- Add `import Data.Bytes` (and maybe `import Data.ByteArray`) at the top of your file
- run idris with `-p bytes` (typically `idris -p idris-multihash -p bytes`)


In the future it would be possible to avoid this dependency by exposing a `String` based API by default and the `Bytes` 
API as an additional import
