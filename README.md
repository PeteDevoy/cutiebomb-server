# Cutie Bomb Server
My excuse to learn Erlang -- an attempt at writing a TCP/IP socket server for the old flash game Dinky Bomb.  I have chosen to call this Cutie Bomb Server because I don't fancy infringing any trade marks.

## What I have discovered

* Dinky Bomb communicates using Flash's XMLSocket class
* Messages are sent as XML elements )(see: `docs/elements.xml`) and terminated by a null byte (`\x00`).