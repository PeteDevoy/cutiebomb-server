# Cutie Bomb Server
My excuse to learn Erlang -- an attempt at writing a TCP/IP socket server for the old flash game Dinky Bomb.  I have chosen to call this Cutie Bomb Server because I don't fancy infringing any trade marks.

## What I have discovered

* Dinky Bomb communicates using Flash's XMLSocket class
* Messages are sent as XML elements (see: `docs/elements.xml`) and terminated by a null byte (`$\0`).
* The cuties on each team are enumerated such that it is odds vs evens (DefineSprite 1022 > frame 1 > DoAction:19)

## Roadmap

- [x] get the game to run on localhost
- [x] mock XML for user interactions
- [x] figure out inter-process communication & global state. [ets](http://erlang.org/doc/man/ets.html)?
- [x] ~replace mock data with real data~
- [ ] implement all events/states
- [ ] implement any timeouts and resource clean-up required
- [ ] test
- [ ] legal due dil/permission seeking
- [ ] deploy somewhere, for great justice

## Development

### Useful tools
* [JPEXS Free Flash Decompiler](https://github.com/jindrapetrik/jpexs-decompiler)
* [Wireshark](https://www.wireshark.org/)

### Build

```bash
./rebar3 compile
./rebar3 release
```

### Start a web server

A web server is needed to avoid browser resource security problems.
Python3 has a built-in web server:

```bash
cd cutiebomb-server
python -m http.server
```

### Start the game server
```bash
cd cutiebomb-server
./_build/default/rel/cbombserver/bin/cbombserver console
```

### State Transition Table
(Work in progress)

| Current State     | Input                | Next State     | Output                           | Done
|-------------------|----------------------|----------------|----------------------------------|------
| flash_policy_wait | request_flash_policy | login_wait     | send policy doc                  | ☐
| login_wait        | connect              | lobby_lurk     | sub to lobby, tx addUser tag     | ☑
| lobby_lurk        | challenge_send       | challenger     | send challenge to opponent       | ☐
| lobby_lurk        | challenge_receive    | challengee     | none                             | ☐
| challenger        | challenge_cancel     | lobby_lurk     | send challenge cancellation      | ☐
| challengee        | challenge_tx_accept  | terrain_choice | send addUserToService to both    | ☑
| challengee        | challenge_tx_decline | lobby_lurk     | send challenge decline           | ☐
| challenger        | challenge_rx_accept  | terrain_choice | addUsersToService                | ☑
| challenger        | challenge_rx_decline | lobby_lurk     | none                             | ☐
| terrain_choice    | terrain_tx_choice    | game           | send selection                   | ☐
| terrain_choice    | terrain_rx_choice    | game           | none                             | ☐
| game_wait         | recording_rx         | game           | none                             | ☑
| game_turn         | recording_tx         | game           | send recording                   | ☑
| game              | surrender_rx         | game           | TODO                             | ☐
| game              | surrender_tx         | game           | TODO                             | ☐
| game              | exit_to_lobby        | lobby_lurk     | sub to lobby, tx addUser tag     | ☑
