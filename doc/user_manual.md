
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego.

## Opis interfejsu użytkownika

Po uruchomieniu programu pojawi się komunikat:

```
////////////////////////////////////////////
/////    Glad to see you in our app!   /////
////////////////////////////////////////////
You can use the following commands:
login     to log in to the server
help      to view this again
exit      to exit the app
``` 

Po wybraniu opcji `login` pojawi się komunikat:

`Please input your username: `

Po którym należy podać nazwę użytkownika.

W przypadku poprawnego połączenia pojawi się komunikat:

```
Hello <Username>!
You have been successfully logged in!
You can use the following commands:
logout    to log out from the server
help      to view this again
exit      to exit the app
```

Jeżeli wystąpi błąd, pojawi się komunikat:

```
Username already logged on
```
 
Po wybraniu opcji `logout` klient rozłączy się z serwerem i powróci do ekranu początkowego, co potwierdzi komunikat:

```
You have been successfully logged out!

////////////////////////////////////////////
/////    Glad to see you in our app!   /////
////////////////////////////////////////////
You can use the following commands:
login     to log in to the server
help      to view this again
exit      to exit the app
```

Jeżeli wystąpi błąd to pojawi się komunikat:

```
This name doesn't exist!
```

Po wybraniu opcji `exit` klient wyjdzie z aplikacji, co potwierdzi komunikat:

```
See you later!    
```    

W przypadku wpisania nieznanej komendy pojawi się komunikat:

```
Not available command.
```

## Polecenia interfejsu użytkownika

### login - utworzenie użytkownika 

Użytkownik wpisuje polecenie `login`, a następnie podaje [Username]. Klient zaloguje użytkownika na serwerze.

### help - wyświetlenie dostępnych komend wraz z opisem
Użytkownik wpisuje polecenie `help`. Klient wyświetli wszystkie dostępne komendy wraz z opisem funkcjonalności.

### logout - wylogowanie użytkownika

Użytkownik wpisuje polecenie `logout`. Klient wylogowuje użytkownika z serwera i prezentuje początkowy ekran logowania.

### exit - wyjście z aplikacji

Użytkownik wpisuje polecenie `exit`. Klient wylogowuje użytkownika z serwera i kończy działanie aplikacji.

