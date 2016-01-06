lsm-hs
==========
LSM implementation in Haskell.

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
For more information on working with Nix and Cabal see:
http://funloop.org/post/2015-02-10-using-nix-from-arch.html

## Building and Installing Using Stack
```
stack build

# to try the library
stack repl

# to run the tests
stack test 2> lsm.log
```

## Examples
Examples can be found in the `examples` directory.

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
During a write, the database will first write the key value pair into both the log file (see [below](#transaction-log-files-and-recovery)) and C0, this is quick because it only happens in memory.
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

### Transaction Log Files and Recovery
The recovery feature is used for recovering data after experiencing power failure, errors or crashes.
We accomplish this by writing new key-value pairs into a log file (e.g. `memtable.log`) on every write request.
When the threshold is reached, but before merging begins, we rename `memtable.log` to `imemtable.log`, `imemtable.log`, should be read only.
In other words, `memtable.log` should be in sync with C0 (the memtable) and `imemtable.log` should be in sync with I0 (the immutable memtable).
When merging completes, `imemtable.log` is deleted.

We assume the Haskell write functions (and the underyling system calls) perform atomic writes.
That is, either the full key-value pair is appended to the log file or nothing is appended.

The recovery is performed when opening the database.
If `imemtable.log` exists, that implies the merging process was interrupted and C1 is in a bad state.
So we read `imemtable.log` and merge it with the current C1.
`imemtable.log` is deleted upon completion.
If `memtable.log` exists, that implies the database did not shut down correctly and data in C0 was not written to C1.
In this case we do the same thing - read `memtable.log` and then mergeit with C1.
The backup log files should be checked first, because the recovery should be done in the order of transaction history.
Note that the recovery should be done in foreground, and must not modify any log files.
If the recovery process crashes (due to an external factor), we should be able to open the database and perform the recovery again, the recovery should succeed if there are no further crashes.

The log files has the following format.
It's a concatination of entries, where every entry contains the length of the data, the data itself, and the SHA256 checksum of the data.
There should always be an even number of entries, i.e. key is followed by value.
```
log file - binary string of entries:
[entry]

entry - represents key or value:
[length (64 bits)] + [data (length bits)] + [sha256 (256 bits)]

```






