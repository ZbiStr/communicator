
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego.

## Opis interfejsu użytkownika

Po uruchomieniu programu ukaże się komunikat:

    choose one command below:
    help
    login
    exit

Po wybraniu opcji **help** pojawi się komunikat:

    You can use the commands below:
    LOGIN     Allows you to log in to our app
    LOGOUT    Allows you to log out of our app

Po wybraniu opcji **login** należy podać nazwę użytkownika i klient połączy się z serwerem.

Po połączeniu widoczny będzie na ekranie znak zachęty jak pokazano niżej:

    Hello <Name>

Jeżeli wystąpi błąd to pojawi się komunikat:

    Username already logged on
 
Po wybraniu opcji **logout** klient rozłączy się z serwerem, co potwierdzi komunikat:

    You have been successfully logged out!

Jeżeli wystąpi błąd to pojawi się komunikat:

    This name does not exiist!

## Polecenia interfejsu użytkownika

### login - utworzenie użytkownika 

Użytkownik wpisuje polecenie "login [dowolna nazwa]". Klient zaloguje użytkownika na serwerze.

### help - wyświetlenie dostępnych komend wraz z opisem
Użytkownik wpisuje polecenie "help". Klient wyświetli wszystkie dostępne komendy wraz z opisem funkcjonalności.

### logout - wylogowanie użytkownika

Użytkownik wpisuje polecenie "logout". Klient wylogowuje użytkownika na serwerze i prezentuje początkowy ekran logowania.

