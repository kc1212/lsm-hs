LevelDB HS
==========
Pure Haskell implementation of LevelDB.

## Install Using Nix
```
cabal2nix --shell . > shell.nix # optional if shell.nix is already up to date
nix-shell
cabal configure --enable-tests
cabal build
```
for more information on working with Nix and Cabal see:
http://funloop.org/post/2015-02-10-using-nix-from-arch.html

## Literature
* [Reviewing LevelDB](http://ayende.com/blog/161410/reviewing-leveldb-part-i-what-is-this-all-about)
