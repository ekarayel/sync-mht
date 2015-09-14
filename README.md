# sync-mht

[![Build Status](https://travis-ci.org/ekarayel/sync-mht.svg?branch=master)](https://travis-ci.org/ekarayel/sync-mht)
[![codecov.io](http://codecov.io/github/ekarayel/sync-mht/coverage.svg?branch=master)](http://codecov.io/github/ekarayel/sync-mht?branch=master)
[![Hackage](https://img.shields.io/hackage/v/sync-mht.svg)](http://hackage.haskell.org/package/sync-mht)
## Synopsis
Fast incremental file transfer using Merkle-Hash-Trees

## Description
A command line tool that can be used to incrementally synchronize a directory hierarchy with a
second one. It is using a Merkle-Hash-Tree to compare the folders, such that the synchronization
time and communication (round) complexity grows only logarithmically with the size of the
directories (assuming the actual difference of the directories is small).

The communication happens through standard streams between parent and child processes, which can
easily be routed through remote command execution tools.

## Examples
The following command

    sync-mht -s foo/ -d bar

will synchronize the local folder bar/ with the local folder foo/, but

    sync-mht -s foo/ -d remote:/bar -r "ssh fred@example.org sync-mht"

will synchronize the folder bar/ in the home directory of the user fred on the host machine
example.org with the local folder foo/.

It is also possible to use it with docker, e.g.

    sync-mht -s foo/ -d remote:/bar -r "docker run -i --volumes-from bar ekarayel/sync-mht sync-mht"

to synchronize the folder /bar (of the container named bar) with the local folder foo/.

Note that: The options -a -u --delete respectively, allow copying of files to the target directory,
updating files that are already in the target directory - not matching the contents in the source
directory and deleting files that are in the destination directory but not in the source directory.
