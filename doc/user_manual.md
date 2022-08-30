
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego.

## Opis interfejsu użytkownika

Po uruchomieniu programu pojawi się komunikat:

```
Welcome to communicator erlang
Please input your username:
``` 

Po którym należy podać nazwę użytkownika. 
Jeśli nazwa jest zabezpieczona hasłem, pojawi się komunikat:

```
This user is password protected, please insert password:
```

Serwer sprawdzi, czy osiągnięta została maksymalna liczba zalogowanych użytkowników. Jeśli tak, pojawi się komunikat:

```
Maximum number of logged in clients reached
```

Jeśli nie, to serwer sprawdzi, czy użytkownik o tej nazwie nie jest aktualnie zalogowany. Jeśli jest, pojawi się komunikat:

```
Username already logged on
```

Jeśli nie, to w przypadku gdy wymagane było podanie hasła, serwer sprawdzi jego poprawność.

W przypadku gdy podano złe hasło, pojawi się komunikat:

```
Wrong password
```

W przypadku podania poprawnego hasła lub gdy nie było ono wymagane, nastąpi zalogowanie.  
Po zalogowaniu pojawi się komunikat

```
Connected to server
For avaiable commands type "help"
```
oraz znak zachęty:

```
@Username>
```

W przypadku poprawnego połączenia, można wpisać polecenie `help`. Pojawi się wtedy komunikat:

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

Po wybraniu opcji `send`  pojawi się znak zachęty:

```
Message>
```
Po wpisaniu dowolnego dozwolonego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który prześle ją do wszystkich zalogowanych użytkowników. Pojawi się wtedy również komunikat:

```
You sent a message to all users
```

Po wybraniu opcji `send <Username>`  pojawi się znak zachęty:

```
Message>
```

Po wpisaniu dowolnego dozwolonego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który sprawdzi, czy dany użytkownik <Username> istnieje w systemie. Jeśli tak, to prześle ją do serwera. Pojawi się wtedy komunikat:

```
You sent a message to <Username>
```
  
W przypadku, gdy użytkownik <Username> istnieje i jest aktualnie zalogowany, serwer prześle do niego wiadomość. Jeśli użytkownik <Username> nie jest zalogowany, ale jest zarejestrowany (zabezpieczony hasłem), wysłana wiadomość zostanie zapisana do jego mailboxa i wysłana po zalogowaniu.
  
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
Only registered users have access to messagess history.
```

w przypadku użytkownika zarejestrowanego, wyświetlona zostanie historia wiadomości. Każda wiadomość wyświetli się w formacie:

```
Year/Month/Day Time - From: Message
```

lub, jeśli skrzynka odbiorcza jest pusta, pojawi się komunikat:

```
Your history is empty.
```

Po wybraniu opcji `logout` klient rozłączy się z serwerem. Pojawi się komunikat:

```
You have been successfully logged out  
```

Nastąpi powrót do ekranu początkowego.  
  
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
użytkownika bez przypisanego hasła wyświetlony zostanie odpowiedni komunikat.

### logout - wylogowanie użytkownika

Użytkownik wpisuje polecenie `logout`. Klient wylogowuje użytkownika z serwera i prezentuje początkowy ekran logowania.

### exit - wyjście z aplikacji

Użytkownik wpisuje polecenie `exit`. Klient wylogowuje użytkownika z serwera i kończy działanie aplikacji.



