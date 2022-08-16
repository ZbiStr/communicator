
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

```
Please input your username: 
```

Po którym należy podać nazwę użytkownika.

W przypadku poprawnego połączenia pojawi się komunikat:

```
Hello <Username>!
You have been successfully logged in!

You can use the following commands:
logout          to log out from the server
message all     to send message to all users
help            to view this again
exit            to exit the app
```

Jeżeli nazwa będzie zajęta, pojawi się komunikat:

```
Username already logged on
```

Po wybraniu opcji `message all`  pojawi się komunikat:

```
Chat with all users started. Type quit to go back to the main menu
>
```
Po wpisaniu dowolnego ciągu znaków, wiadomość zostanie przekazana do serwera, który prześle ją do wszystkich zalogowanych użytkowników.

Aby wyjść z czatu, należy wpisać `quit`, pojawi się komunikat:

```
Do you want to quit? (y/n)
```
Po wpisaniu `y` powrócimy do menu zalogowanego użytkownika.

Po wpisaniu `n` powrócimy do czatu.

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

### message all - wysłanie wiadomości do wszystkich zalogowanych użytkowników

Użytkownik wpisuje polecenie `message all`. Pojawia się okno czatu, z którego można wysłać wiadomości do wszystkich zalogowanych użytkowników.

### exit - wyjście z aplikacji

Użytkownik wpisuje polecenie `exit`. Klient wylogowuje użytkownika z serwera i kończy działanie aplikacji.

