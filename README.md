LevelDB HS
==========
Pure Haskell implementation of LevelDB.

## Building and Installing Using Nix
```
cabal2nix --shell . > shell.nix # optional if shell.nix is already up to date
nix-shell
cabal configure --enable-tests
cabal build

# to try the library in REPL
cabal repl

# to install in a sandbox
cabal sandbox init
cabal install
```
for more information on working with Nix and Cabal see:
http://funloop.org/post/2015-02-10-using-nix-from-arch.html

## Literature
* [Reviewing LevelDB](http://ayende.com/blog/161410/reviewing-leveldb-part-i-what-is-this-all-about)
* [SSTable and Log Structured Storage: LevelDB](https://www.igvita.com/2012/02/06/sstable-and-log-structured-storage-leveldb/)

