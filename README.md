# Cutie Bomb Server
My excuse to learn Erlang -- an attempt at writing a TCP/IP socket server for the old flash game Dinky Bomb.  I have chosen to call this Cutie Bomb Server because I don't fancy infringing any trade marks.

## What I have discovered

* Dinky Bomb communicates using Flash's XMLSocket class
* Messages are sent as XML elements (see: `docs/elements.xml`) and terminated by a null byte (`\x00`).

## State Transition Table
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
