# Chat server

This simple starting versioon of the chat server will allow to users to connect and it echoes what the users sends to the server.

## How to run locally

```
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
`create:<room_name>`: will create a room with the provided name. You still have to join the room after creating.
`destroy:<room_name>`: will delete the room and kick everyone in it, if you are the room creator.
`join:<room_name>`: will join the room.
`leave`: will kick you out of the room you are currently in.
`whisper: <username> <message>`: will send a message to the provided user.
`private: <room_name>`: will create a private room.
`ínvite: <username>`: will invite an user. The user will be notified and can join the room. To invite an user, you have to be in the room.

# Notes

I left `io:format` calls inside the server because it's useful for development.
