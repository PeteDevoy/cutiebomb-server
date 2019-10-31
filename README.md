# Cutie Bomb Server
My excuse to learn Erlang -- an attempt at writing a TCP/IP socket server for the old flash game Dinky Bomb.  I have chosen to call this Cutie Bomb Server because I don't fancy infringing any trade marks.

## What I have discovered

* Dinky Bomb communicates using Flash's XMLSocket class
* Messages are sent as XML elements (see: `docs/elements.xml`) and terminated by a null byte (`$\0`).
* The cuties on each team are enumerated such that it is odds vs evens (DefineSprite 1022 > frame 1 > DoAction:19)

## Roadmap

- [x] get the game to run on localhost
- [ ] mock XML for user interactions
- [ ] figure out inter-process communication & global state. [ets](http://erlang.org/doc/man/ets.html)?
- [ ] replace mock data with real data
- [ ] implement any timeouts and resource clean-up required
- [ ] deploy somewhere, for great justice

## Development

### Useful tools
* [JPEXS Free Flash Decompiler](https://github.com/jindrapetrik/jpexs-decompiler)
* [Wireshark](https://www.wireshark.org/)

### Build
TODO.

### Start a web server

A web server is needed to avoid browser resource security problems.
Python3 has a built-in web server:

```bash
cd cutiebomb-server
python -m http.server
```

### Start the game server
Start an erlang shell:
```bash
cd cutiebomb-server
erl
```

At the erlang shell's `1>` prompt:

```
cbomb:start(normal, []).
```

### Mocking incoming XML
As a starting point `cbomb_xml.erl` is giving back mostly mock data.

Chat messages are used to pull XML messages which, in production, would be
pushed in response to server or other-user events.

To invoke the mock XML log-in with username `debug` and send a chat message:

| Chat message | XML Sent to client                                                                                                |
|--------------|-------------------------------------------------------------------------------------------------------|
| addUser      | `<addUser username="mrwhite" avatar="1\|GB" userid="2"/>`                                        |
| rxChallenge  | `<invite message="0" gameTypeId="" userid="2" targetUserId="1" username="mrwhite" avatar="1"/>` |
| addMrWhiteToService  | `<addedToService username="mrwhite" userid="2"/>` |


### State Transition Table
(Work in progress)

| Current State     | Input                | Next State     | Output                           |
|-------------------|----------------------|----------------|----------------------------------|
| flash_policy_wait | request_flash_policy | login_wait     | send policy doc                  |
| login_wait        | connect              | lobby_lurk     | send "ENTERED THE LOBBY" chat    |
| lobby_lurk        | challenge_send       | challenger     | send challenge to another player |
| lobby_lurk        | challenge_receive    | challengee     | none                             |
| challenger        | challenge_cancel     | lobby_lurk     | send challenge cancellation      |
| challengee        | challenge_tx_accept  | terrain_choice | send challenge accept            |
| challengee        | challenge_tx_decline | lobby_lurk     | send challenge decline           |
| challenger        | challenge_rx_accept  | terrain_wait   | none                             |
| challenger        | challenge_rx_decline | lobby_lurk     | none                             |
| terrain_choice    | terrain_tx_choice    | game_wait      | send selection                   |
| terrain_wait      | terrain_rx_choice    | game_turn      | none                             |
| game_wait         | recording_rx         | game_turn      | none                             |
| game_turn         | recording_tx         | game_wait      | send recording                   |
| TODO              |                      | game_over      | none                             |
| TODO              | surrender_rx         |                |                                  |
| TODO              | surrender_tx         |                |                                  |
| game_over         | exit_to_lobby        | lobby_lurk     | send "ENTERED THE LOBBY" chat    |
