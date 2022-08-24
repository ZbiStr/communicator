
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego.

## Opis interfejsu użytkownika

Po uruchomieniu programu pojawi się komunikat:

```
Welcome to communicator erlang
Please input your username:
``` 

Po którym należy podać nazwę użytkownika. Serwer sprawdzi, czy użytkownik o tej nazwie nie jest 
aktualnie zalogowany. Jeśli jest, pojawi się komunikat:

```
Username already logged on
```
Jeśli nie, serwer sprawdzi, czy użytkownik o tej nazwie jest chroniony hasłem. Jeśli nie, nastąpi
zalogowanie, oraz pojawi się komunikat:

```
Connected to server
For avaiable commands type "help"
```
oraz znak zachęty:

```
@Username>
```

Jeśli nazwa jest zabezpieczona hasłem, pojawi się komunikat:

```
This user is password protected, please insert password
```

Po wpisaniu hasła i naciśnięciu enter, hasło zostanie wysłane do serwera, który sprawdzi jego poprawność.
Jeśli hasło jest poprawne, nastąpi zalogowanie

W przypadku gdy podano złe hasło, pojawi się komunikat:

```
Wrong password
Please input your username:
```

W przypadku poprawnego połączenia, można wpisać polecenie `help`. Pojawi się wtedy komunikat:

```
You can use the following commands:
logout                  to log out from the server
send                    to send a message to all users
send Username   to send a message to user called Username
users                   to show the list of active users
set_pass                to set a new password
history                 to see your message history (only for registered users)
help                    to view this again
exit
```

Po wybraniu opcji `send`  pojawi się znak zachęty:

```
Message >
```
Po wpisaniu dowolnego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który prześle ją do wszystkich zalogowanych użytkowników. Pojawi się wtedy również komunikat:

```
You sent a message to all users
```

Po wybraniu opcji `send <Username>`  pojawi się znak zachęty:

```
Message >
```

Po wpisaniu dowolnego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który sprawdzi, czy dany użytkownik <Username> istnieje w systemie. Jeśli tak, to prześle ją do serwera. Pojawi się wtedy komunikat:

```
You sent a message to <Username>
```
  
W przypadku, gdy użytkownik <Username> istnieje i jest aktualnie zalogowany, serwer prześle do niego wiadomość. Jeśli użytkownik <Username> nie jest zalogowany, ale jest zarejestrowany (zabezpieczony hasłem), wysłana wiadomość zostanie zapisana do jego mailboxa.
  
W przypadku, gdy użytkownik nie istnieje w systemie, pojawi się komunikat:
  
```
There is no such user!
```

Po wybraniu opcji `users`  wyświetlona zostanie lista aktualnie zalogowanych użytkowników:

```
List of active users: [<Username1>, <Username2>, ...]
```
  
Po wybraniu opcji `set_pass`  pojawi się komunikat:

```
Please input desired password:
```
  
Po którym należy podać hasło które chcemy ustawić. Po wpisaniu dowolnego ciągu znaków i naciśnięciu `enter` hasło zostanie przekazane do serwera, który przypisze je do aktualnie zalogowanego użytkownika. Od tej pory przy logowaniu wymagane będzie podanie przypisanego hasła. Pojawi się wtedy również komunikat:
  
```
Password has been set
```

Po wybraniu opcji `history`, w przypadku użytkownika niezarejestrowanego, pojawi się komunikat:

```
You have access to messagess history only from registered account.
```

w przypadku użytkownika zarejestrowanego, wyświetlona zostanie historia wiadomości. Każda wiadomość wyświetli się w formacie:

```
Year/Month/Day Time - From: Message
```

lub, jeśli skrzynka odbiorcza jest pusta, pojawi się komunikat:

```
Your history is empty.
```

Po wybraniu opcji `logout` klient rozłączy się z serwerem i powróci do ekranu początkowego, co potwierdzi komunikat:

```
You have been successfully logged out

Welcome to communicator erlang
Please input your username:
```

Po wybraniu opcji `exit` klient wyjdzie z aplikacji, co potwierdzi komunikat:

```
** exception exit: normal   
```    

W przypadku wpisania nieznanej komendy pojawi się komunikat:

```
Not a viable command
```

## Polecenia interfejsu użytkownika

### help - wyświetlenie dostępnych komend wraz z opisem
Użytkownik wpisuje polecenie `help`. Klient wyświetli wszystkie dostępne komendy wraz z opisem funkcjonalności.

### send - wysłanie wiadomości do wszystkich zalogowanych użytkowników

Użytkownik wpisuje polecenie `send`. Pojawia się znak zachęty, po wpisaniu treści wiadomości można ją wysłać do wszystkich zalogowanych i zarejestrowanych użytkowników.
  
### send <Username>- wysłanie wiadomości do użytkownika <Username>

Użytkownik wpisuje polecenie `send <Username>`. Pojawia się znak zachęty, po wpisaniu treści wiadomości można ją wysłać do użytkownika <Username> jeśli jest zalogowany lub zarejestrowany.
  
### users- wyświetlenie listy zalogowanych użytkowników

Użytkownik wpisuje polecenie `users`. Pojawia się lista zalogowanych użytkowników.
  
### set_pass- ustawienie hasła do aktualnie zalogowanego użytkownika

Użytkownik wpisuje polecenie `set_pass`. Użytkownik wpisuje hasło, które zostaje do niego przypisane. Podanie hasła będzie wymagane przy każdym kolejnym zalogowaniu.

### history - wyświetlenie historii wiadomości

Użytkownik wpisuje polecenie `history`. W przypadku użytkownika z przypisanym hasłem wyświetlona zostaje historia wiadomości. W przypadku 
użytkownika bez przypisanego hasła wyświetlony zostanje odpowiedni komunikat.

### logout - wylogowanie użytkownika

Użytkownik wpisuje polecenie `logout`. Klient wylogowuje użytkownika z serwera i prezentuje początkowy ekran logowania.

### exit - wyjście z aplikacji

Użytkownik wpisuje polecenie `exit`. Klient wylogowuje użytkownika z serwera i kończy działanie aplikacji.



