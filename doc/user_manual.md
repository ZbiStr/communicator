
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego.

## Opis interfejsu użytkownika

Po uruchomieniu programu ukaże się komunikat:

    ////////////////////////////////////////////
    /////    Glad to see you in our app!   /////
    ////////////////////////////////////////////
    //       Choose one command below:        //
    // help                                   //
    // login                                  //
    // exit                                   //
    ////////////////////////////////////////////
    
Po wybraniu opcji **help** pojawi się komunikat:

    You can use the commands below:
    LOGIN     Allows you to log in to our app
    EXIT      Allows you to exit the app

Po wybraniu opcji **login** należy podać nazwę użytkownika i klient połączy się z serwerem.

W przypadku poprawnego połączenia pojawi się komunikat:

    Hello <Username>!
    ////////////////////////////////////////////
    //       Choose one command below:        //
    // help                                   //
    // logout                                 //
    // exit                                   //
    ////////////////////////////////////////////

Jeżeli wystąpi błąd, pojawi się komunikat:

    Username already logged on
    
Po wybraniu opcji **help** pojawi się komunikat:

    You can use the commands below:
    LOGOUT    Allows you to log out of our app
    EXIT      Allows you to exit the app
 
Po wybraniu opcji **logout** klient rozłączy się z serwerem i powróci do ekranu początkowego, co potwierdzi komunikat:

    You have been successfully logged out!
    
    ////////////////////////////////////////////
    /////    Glad to see you in our app!   /////
    ////////////////////////////////////////////
    //       Choose one command below:        //
    // help                                   //
    // login                                  //
    // exit                                   //
    ////////////////////////////////////////////

Jeżeli wystąpi błąd to pojawi się komunikat:

    This name does not exist!
    
Po wybraniu opcji **exit** klient wyjdzie z aplikacji, co potwierdzi komunikat:

    See you later!    
    

## Polecenia interfejsu użytkownika

### login - utworzenie użytkownika 

Użytkownik wpisuje polecenie "login", a następnie podaje [Username]. Klient zaloguje użytkownika na serwerze.

### help - wyświetlenie dostępnych komend wraz z opisem
Użytkownik wpisuje polecenie "help". Klient wyświetli wszystkie dostępne komendy wraz z opisem funkcjonalności.

### logout - wylogowanie użytkownika

Użytkownik wpisuje polecenie "logout". Klient wylogowuje użytkownika z serwera i prezentuje początkowy ekran logowania.

### exit - wyjście z aplikacji

Użytkownik wpisuje polecenie "exit". Klient wylogowuje użytkownika z serwera i kończy działanie aplikacji.

