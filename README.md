# sync-mht

## Synopsis
Fast incremental file transfer using Merke-Hash-Trees

## Description
A command line tool that can be used to incrementally synchronize a directory hierarchy with a
second one. It is using a Merkle-Hash-Tree to compare the folders, such that the synchronization
time and communication (round) complexity grows only logarithmically with the actual size of the
directories (assuming the actual difference of the directories is small).

The communication happens through standard streams between parent and child processes, which can
easily be routed through remote command execution tools, e.g.

    sync-mht -s foo/ -d bar

will synchronize the local folder bar/ with the local folder foo/, but

    sync-mht -s foo/ -d remote:bar -r "ssh fred@example.org sync-mht"

will synchronize the folder bar/ in the home directory of the user fred on the host machine
example.org with the local folder foo/.

It is also possible to use it with docker, e.g.

    sync-mht -b foo/ -d remote:bar -r "docker run -i --volumes-from bar ekarayel/sync-mht sync-mht"

to synchronize the folder /bar (of the container named bar) with the local folder foo/.

