# Chat server

This simple starting versioon of the chat server will allow to users to connect and it echoes what the users sends to the server.

## How to run locally

```
rebar3 compile
rebar3 shell
```

# To connect to the server:

```
nc localhost 1234
```

You will be asked your name and then you can execute any of these commands:

`say:<message>`: will send a message to everyone in the current room you are. If you're not in a room, you will send the message globally.
`whoami`: will return your username.
`whereami`: will return your current room.
`create:<room_name>`: will create a room with the provided name.
`destroy:<room_name>`: will delete the room and kick everyone in it, if you are the room creator.
`join:<room_name>`: will join the room.
`leave`: will kick you out of the room you are currently in.

# Notes

The `accept` function should be run on the handler to handle concurrent clients trying to login at the same time.
