# Liar's dice game

Simple [Liar's dice](https://www.officialgamerules.org/liarsdice) simulator and bot battleground.

## How to install

If you want to run simulator locally, installing is as simple as:
```sh
wget https://github.com/dimchee/LiarsDice/releases/download/latest/liarsdice
```
Now you can run server with:
```sh
./liarsdice
```

## How to play manually

`WIP`

## Example Bots

- If you have [node](https://nodejs.org/en) installed, you can run example bot from `Bots` folder with command:
```sh
Bots/run.sh
```

## Bot API

- Communication is established using `TCP/IP sockets` and `JSON` messages, so you can use your preferred stack
- Communication is **synchronous**, you receive request, you sent answer, and then repeat
- Response and a request **have to share** `message_id` field

### Message formats

We use `TypeScript` notation for `JSON` specification:

```ts
type Integer = number;
type Ordinal = Integer; // starting from 1
type PlayerID = string;
type Face = 1 | 2 | 3 | 4 | 5 | 6;
type Count = Integer;
type Bid = [Face, Count];
type EmptyBid = [0, 0];
```

#### Server to client

```ts
export interface outbound {
    message_id: string;
    game_number: Ordinal;
    round_number: Ordinal;
    move_number: Ordinal;
    your_hand: Face[];
    other_hands: [PlayerID | "yourself", Count][]; // sorted by playing order
    last_move: "first_move" | "bid_made" | "challenge_made" | "invalid_move";
    last_bid: Bid | EmptyBid;
    last_bidder: PlayerID | "not_available";
    last_loser: PlayerID | "not_available";
    last_challenger: PlayerID | "not_available";
}
```

#### Client to server

On connecting to server you can send (but don't need to!) your name:
```ts
export interface metadata {
    name: string
}
```

If it's your turn:
```ts
export interface inbound {
    message_id: Integer;
    move: "challenge" | Bid;
}
```

Otherwise:
```ts
export interface inbound {
    message_id: Integer;
    move: "pass" | "challenge";
}
```

## Simulation details

- Simulator is using **Round-robin** principle in **playing order** of **current round** when 
resolving dilemmas. When **all** client messages are collected, if there is one:
    - first invalid move in **playing order** is acknowledged
    - first challenge in **playing order** is acknowledged
- Lastly, **The** bid of player whose hand it is gets acknowledged

## Bots 

- Python, JavaScript, C++

## Notes on Server code

- This project is part of my never ending `haskell` learning endeavour, so I am trying to use 
as many awesome features as I can.
- Some of cool libraries I am using:
    - [`lens`](http://lens.github.io/)
    - [`STM`](https://wiki.haskell.org/Software_transactional_memory)

## Mentions

- Specification developed in collaboration with [andrijast](https://github.com/andrijast),
he has [similar project](https://github.com/andrijast/liars-dice/tree/main)
- [online AI](https://dudo.ai/)
