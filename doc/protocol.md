# Protokół

Niniejszy dokument zawiera opis protokołu komunikacyjnego pomiędzy serwerem a klientem.

## Sposób komunkacji

Cała komunikacja odbywa się w środowisku erlang OTP 24.3.4.

## Przesyłane komunikaty

Poniżej znajduje się opis wysyłanych komunikatów.

### Login

```mermaid
sequenceDiagram
Klient ->> Serwer: login(Username, Address)
alt success
Serwer ->> Klient: ok
else error
Serwer ->> Klient: {error, already_exists}
end
```
Klient wysyła do serwera wiadomość w postaci login(Username, Address). Jeżeli zapytanie zostało poprawnie przetworzne, serwer odpowiada "ok". Jeżeli nastąpił błąd to serwer odpowiada "{error, already_exists}".

### Logout

```mermaid
sequenceDiagram
Klient ->> Serwer: logout(Username)
alt success
Serwer ->> Klient: ok
else error
Serwer ->> Klient: {error, does_not_exist}
end
```
Klient wysyła do serwera wiadomość w postaci logout(Username). Jeżeli zapytanie zostało poprawnie przetworzne, serwer odpowiada "ok". Jeżeli nastąpił błąd to serwer odpowiada "{error, does_not_exist}".

### Message all

```mermaid
sequenceDiagram
Klient1 ->> Serwer: send_message(From, To, Message)
Serwer ->> Serwer: zapisanie wiadomości
par Serwer to Klient2
Serwer->>Klient2: {From, Message}
and Serwer to Klient3
Serwer->>Klient3: {From, Message}
end
```
Klient wysyła do serwera wiadomość w postaci send_message(From, To, Message). Jeżeli zapytanie zostało poprawnie przetworzne, serwer wysyła wiadomość do wszystkich zalogowanych użytkowników w postaci {From, Message}.
