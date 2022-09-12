# User manual

The document demonstrates the user guide for the client of the communicator. The format of messages is dependent on the language a user that the user has chosen (PL/EN).
>**Disclaimer:** The only characters allowed are printable ASCII characters.

## User interface description

After starting the program, the following message will appear:

```
Welcome to communicator erlangpol!
Please input your username:
``` 

Then you need to enter your username.
If the username is password protected, the following message will appear:

```
This user is password protected, please insert password:
```

The server checks, if the maximum number of users has been reached. If so, the following message will appear:

```
Maximum number of logged in clients reached
```

If not, the server checks, if a user with the same username is currently logged in. If it is, the following message will appear:

```
Username already logged on
```

If not, then if a password was required, the server will check its correctness.

If the wrong password was given, the following message will appear:

```
Wrong password
```

In case of providing the correct password or it was not necessary, a user will be logged in.
After logging in, a message will appear:

```
Connected to server
For avaiable commands type "help"
```
oraz znak zachęty:

```
@Username>
```

If the connection was conducted successfully, it is possible to enter a command `help`. The message will appear:

```
You can use the following commands:
logout                  to log out from the server
send                    to send a message to all users
send Username           to send a message to user called Username
users                   to show the list of active users
set_pass                to set a new password
history                 to see your message history (only for registered users)
help                    to view this again
exit                    to exit the app
```

After selecting `send`, a user will be prompted: 

```
Message>
```

After entering any allowed string and pressing `enter`, the message will be forwarded to the server,  The following message will also appear:

```
You sent a message to <Username>
```

If the user <Username> exists and is currently logged in, the server will send a message. If the user <Username> is not logged in, but is registered (password protected), the sent message will be saved to 