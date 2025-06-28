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

`SAY:<message>`: will send a message to everyone
`WHOMAI`: will return your username
