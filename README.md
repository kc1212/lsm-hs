lsm-hs
==========
LSM implementation in Haskell

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

## Building and Installing Using Stack
```
stack build

# to try the library
stack repl

# to run tests
stack test
```

## Example Usage
```
withLevelDB "/tmp/testdb" options (
    do
        let key = (C8.pack "key1", (0, MT.Value))
        add  key (C8.pack "value")
        res <- get key
        liftIO $ print res
    )
```

## Literature
* [Reviewing LevelDB](http://ayende.com/blog/161410/reviewing-leveldb-part-i-what-is-this-all-about)
* [SSTable and Log Structured Storage: LevelDB](https://www.igvita.com/2012/02/06/sstable-and-log-structured-storage-leveldb/)

## Implementation Notes
The implementation is based on the [LSM paper](http://www.cs.umb.edu/~poneil/lsmtree.pdf) by O'Neil et al.

There are two levels C0 and C1 and one temporary level - I0 (Immutable C0). C0, resides only in memory, is implemented using Data.Map from the containers package (internally it is a balanced binary tree).
C1 is implemented using the [b-tree](https://hackage.haskell.org/package/b-tree) package. It support operations such as lookup, merge, and writing to disk. Those are essential for LSM.
Finally, I0 is the same as C0 except it is immutable.

```

 read   /\                        /\                          /\
 write /  \ copy after threshold /  \  merge in background   /  \
----->/ C0 \------------------->/ I0 \--------------------->/    \
      ------                    ------                     /  C1  \
                                                          /        \
                                                         /          \
                                                         ------------
```
### Write
During a write, the database will first write the key value pair into C0, this is quick because it only happens in memory.
Once the size of C0 reaches a certain threshold, for example 100 MB. C0 is copied to I0 and then C0 is emptied.
A background thread starts and merges the content of I0 with C1, this produces and new C1 and is written to disk.
The state of the database is updated to point to the new C1 after the write completes.
One copy of the old C1 table will be kept on disk. There should be a file, called `CURRENT`, that keeps track of the latest C1.

### Read
When the user wishes to read a value, the program looks into C0, I0 and C1 in that order and returns the first result it finds, otherwise it returns nothing.

### Delete
Deleting a key/value pair is the same as writing an empty string. The merging process should write the 'deleted' key/value pair to C1.

### Open
The database name is the folder name. If the folder does not exist the program should create it and also create an empty C1 file as well as the `CURRENT` file.
If the database exists the program reads the `CURRENT` file and keeps the latest C1 name in its state.

### TODOs
recovery, fast reads, skip list


