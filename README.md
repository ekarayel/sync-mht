# sync-mht

[![Build Status](https://travis-ci.org/ekarayel/sync-mht.svg?branch=master)](https://travis-ci.org/ekarayel/sync-mht)
[![codecov.io](https://img.shields.io/codecov/c/github/ekarayel/sync-mht.svg)](http://codecov.io/github/ekarayel/sync-mht?branch=master)
[![Hackage](https://img.shields.io/hackage/v/sync-mht.svg)](http://hackage.haskell.org/package/sync-mht)
[![Release](https://img.shields.io/github/release/ekarayel/sync-mht.svg)](https://github.com/ekarayel/sync-mht/releases)
[![Stackage LTS 4](http://stackage.org/package/sync-mht/badge/lts-4)](http://stackage.org/lts-4/package/sync-mht)
[![Stackage Nightly](http://stackage.org/package/sync-mht/badge/nightly)](http://stackage.org/nightly/package/sync-mht)

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

By default `sync-mht` will only show a statistic about the difference between the source and
destination directories.  The options `--add`, `--update` and `--delete` respectively allow
copying of files to the target directory, updating files that are already in the target directory -
not matching the contents in the source directory and deleting files that are in the destination
directory but not in the source directory.

## Setup
Installing the latest release of sync-mht with [stack](https://github.com/commercialhaskell/stack):
```
stack install sync-mht
```

## Contribution
If you want to contribute - cloning the latest commit and building can be done using the
following steps:

```
git clone --recursive https://github.com/ekarayel/sync-mht.git
cd sync-mht
stack exec runhaskell configure.hs
stack install
```
