LevelDB HS
==========
Pure Haskell implementation of LevelDB.

## Install Using Nix
```
cabal2nix --shell . > shell.nix # optional if shell.nix is already up to date
nix-shell
cabal sandbox init              # optional if there's already a sandbox
cabal install                   # or build/configure/repl etc
```
for more information on working with Nix and Cabal see:
http://funloop.org/post/2015-02-10-using-nix-from-arch.html




