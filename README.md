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

There are some things I should refactor (and probably a lot more I'm not aware of), but I didn't because of the time.
-I left `io:format` calls inside the server, instead of using proper logging tools.
-Variable and file names are not consistent. `-spec` would have been nice for static analysis.
-I didn't write tests (usually I write tests while developing, but since I didn't know the language and the principles around it, I focused on that).
-I'm sure my architecture is not "OTP compliant" (did I say correctly?), but it would have taken me more time to learn it well and I focused on bringing a working project, even if imperfect.
