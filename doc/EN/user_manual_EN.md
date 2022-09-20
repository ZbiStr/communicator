# User manual

The document demonstrates the user guide for the client of the communicator. The format of messages depends on the language that the user has selected (PL/EN).
>**Disclaimer:** The only characters allowed are printable ASCII characters.

## User interface description

While running the program, the message will appear:

```erlang
Select your language
pl -polski
en -english
Option:
```

A user needs to enter a proper abbreviation of the language they are selecting.

After starting the program, the following message will appear:

```
Welcome to communicator erlangpol!
Please input your username:
``` 

Then you need to enter your username.
After entering the username, the client displays Message Of The Day. This is the default server message:

```
!!!Erlangpol server: In this server we slay our enemies!!!
```


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

If the user <Username> exists and is currently logged in, the server will send a message. If the user <Username> is not logged in, but is registered (password protected), the sent message will be saved to their inbox and send when they are back online.

If there is no such user, there will be a message:

```
There is no such user!
```

> **Disclaimer:** A buffer has been implemented in the program. This means that the terminal is blocked until the user sends the message. It also means that the user will not receive any messages sent by other users during this time.

After selecting the option `users`, a list of currently logged in users will be displayed. 

```
List of active users: [<Username1>, <Username2>, ...]
```

After selecting the option `set_pass`, there will be a message:

```
Please enter your desired password:
```

Then one need to enter their password. After entering any string and pressing `enter`, the password will be conveyed to the server, which will assign it to the currently logged in user. Since then, while logging in, it will be crucial to enter that password. The message will appear:

```
The password has been set
```

After selecting `history`, if a user is not registered, there will be a message:

```
Only registered users have access to messages history
```

If a user is registered, message history will appear. Every message will be displayed in such format:

```
Year/Month/Day Time - From: Message
```

or, if the inbox is empty, the appearing message is the following:

```
Your history is empty
```

After selecting `logout`, klient disconnects with the server. The message will appear:

```
You have been successfully logged out
```

You will be returned to the home screen.

After selecting `exit`, client exits the application, what will be confirmed by the message:

```
** exception exit: normal   
```    

If a user enters unknown command, the message will appear:

```
Not a viable command
```

## User interface commands 

### help - displaying the available commands with their description

A user enters a command `help`. The client will display all available commands along with a description of the functionalities.

### send (s) - sending a message to all logged in users

A user enters a command `send` or `s`. The prompt will be displayed. After entering a message, it can be sent to all logged in and registered users. 

### send (s) <Username> - sending a message to the user <Username>

A user enters a command `send <Username>` or `s <Username>`. The prompt will be displayed. After entering a message, it can be sent to the user <Username>, does not matter if registered or logged in.

### users (us) - printing a list of logged in users

A user enters a command `users`or `us`. A list of all logged in users will be displayed.

### set_pass (sp) - setting a password to the currently logged on user

A user enters a command `set_pass` or `sp`. Then, they enter a password that is assigned to them. The password will be required each time one logs in.

### history (his) - printing messages history

A user enters a command `history`. If there is a user with the assigned password, the message history will be displayed. If there is a user without a password, an appropriate message will be displayed.

### logout (lg) - logging the user out

A user enters a command `logout`. The client logs the user out from the server and displays the initial login screen.

### exit - exiting the app

A user enters a command `exit`. The client logs the user out from the server and exits the application.
