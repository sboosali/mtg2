# `srcdst` **TODO**

## hashing?

hash a stream (via `conduit`).

the hashing operation must be associative; the bytes will be consumed in an indeterminate number/size of chunks.

``` haskell
sinkHash :: (Monad m, HashAlgorithm hash) => Consumer ByteString m (Digest hash)
```

>A Sink that hashes a stream of ByteStrings and creates a digest d.

http://github.com/vincenthz/hs-cryptohash-conduit

## 

