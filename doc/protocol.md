# Protokół

Niniejszy dokument zawiera opis protokołu komunikacyjnego pomiędzy serwerem a klientem.

## Sposób komunkacji

Cała komunikacja odbywa się w środowisku erlang OTP 24.3.4.

## Przesyłane komunikaty

Poniżej znajduje się opis wysyłanych komunikatów.

### login

```mermaid
sequenceDiagram
Klient ->> Serwer: login(Username, Address, Password)
Serwer ->> Serwer: sprawdzenie czy użytkownik posiada hasło
alt success
Serwer ->> Klient: ok
else error
Serwer ->> Klient: wrong_password
else already exists
Serwer ->> Klient: already_exists
end
```

Klient wysyła do serwera wiadomość w postaci login(Username, Address, Password). Jeżeli zapytanie zostało poprawnie przetworzne, a użytkownik nie posiada hasła serwer odpowiada "ok". Jeżeli użytkownik o podanej nazwie już istnieje serwer odpowie "already_exists". W przypadku kiedy użytkownik posiada hasło, będzie musiał je wpisać zaraz po loginie. Jeżeli hasło będzie poprawne serwer odpowie "ok". Jeśli hasło będzie niepoprawne, serwer odpowie "wrong_password".

### logout

```mermaid
sequenceDiagram
Klient ->> Serwer: logout(Username)
alt success
Serwer ->> Klient: ok
else error
Serwer ->> Klient: does_not_exist
end
```
Klient wysyła do serwera wiadomość w postaci logout(Username). Jeżeli zapytanie zostało poprawnie przetworzne, serwer odpowiada "ok". Jeżeli nastąpił błąd to serwer odpowiada "does_not_exist".

### send

```mermaid
sequenceDiagram
participant Klient1
participant Serwer
note right of Klient2: ...
Klient1 ->> Serwer: send_message(From, To, Message)
Serwer ->> Serwer: zapisanie wiadomości
par Serwer to Klient2
Serwer->> Klient2: {From, Message}
and Serwer to KlientN
Serwer->>KlientN: {From, Message}
end
note right of Klient2: ...
```
Klient wysyła do serwera wiadomość w postaci send_message(From, To, Message). Jeżeli zapytanie zostało poprawnie przetworzne, serwer wysyła wiadomość do wszystkich zalogowanych użytkowników w postaci {From, Message}.

### send Username

```mermaid
sequenceDiagram
Klient ->> Serwer: send_message(From, Username, Message)
Serwer ->> Serwer: zapisanie wiadomości
Serwer ->> Serwer: wyszukanie użytkownika
alt success
Serwer ->> Username: {From, Message}
else error
Serwer ->> Klient: does_not_exist
end
```
Klient wysyła do serwera wiadomość w postaci send_message(From, Username, Message). Jeżeli zapytanie zostało poprawnie przetworzne, serwer wysyła wiadomość do użytkownika {From, Message}. Jeżeli nie ma takiego użytwnika serwer odpowiada "does_not_exist".

### users

```mermaid
sequenceDiagram
Klient ->> Serwer: show_active_users() 
Serwer ->> Klient: [ActiveUsers]
```
Klient wysyła do serwera wiadomość w postaci show_active_users(). W odpowiedzi serwer wysyła [ActiveUsers].

### set_pass
```mermaid
sequenceDiagram
Klient ->> Serwer: set_password(Name, Password)  
Serwer ->> Klient: ok
```
Klient wysyła do serwera wiadomość w postaci set_password(Name, Password). W odpowiedzi serwer wysyła "ok".


