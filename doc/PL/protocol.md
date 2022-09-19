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
alt sukces
Serwer ->> Klient: ok
else niepoprawne hasło
Serwer ->> Klient: wrong_password
else użytkownik już istnieje
Serwer ->> Klient: already_exists
else maksymalna liczba użytkowników
Serwer ->> Klient: max_reached
end
```

Klient wywołuje funkcję serwera w postaci login(Username, Address, Password). Jeżeli zapytanie zostało poprawnie przetworzone, serwer odpowiada "ok". Jeżeli użytkownik o podanej nazwie już istnieje serwer odpowie "already_exists". Jeśli hasło będzie niepoprawne, serwer odpowie "wrong_password". Jeśli zostanie osiągnięta maksymalna liczba użytkowników serwer odpowie "max_reached".

### logout

```mermaid
sequenceDiagram
Klient ->> Serwer: logout(Username)
Serwer ->> Klient: ok
```
Klient wywołuje funkcję serwera w postaci logout(Username). Jeżeli zapytanie zostało poprawnie przetworzne, serwer odpowiada "ok".

### send

```mermaid
sequenceDiagram
participant Klient1
participant Serwer
note right of Klient2: ...
Klient1 ->> Serwer: send_message(all, Time, From, Message, MsgId)
par Serwer to Klient2
Serwer->> Klient2: {Time, From, Message, {MsgId, Klient2}}
Klient2 ->> Serwer: {msg_confirm_from_client, {MsgId, Klient2}}
and Serwer to KlientN
Serwer->>KlientN: {Time, From, Message, {MsgId, KlientN}}
KlientN ->> Serwer: {msg_confirm_from_client, {MsgId, KlientN}}
end
Serwer ->> Klient1: {msg_confirm_from_server, MsgId}
note right of Klient2: ...
```

Klient wywołuje funkcję serwera w postaci send_message(all, Time, From, Message, MsgId). Jeżeli zapytanie zostało poprawnie przetworzne, serwer wysyła wiadomość do wszystkich zalogowanych użytkowników w postaci {Time, From, Message, {MsgId, KlientN}}.

### send <Username>

```mermaid
sequenceDiagram
Klient1 ->> Serwer: wyszukanie użytkownika
alt sukces
Klient1 ->> Serwer: send_message(Username, Time, From, Message, MsgId)
Serwer ->> Klient2: {Time, From, Message, MsgId}
par
Klient2->> Serwer: {msg_confirm_from_client, MsgId}
and
Serwer ->> Klient1: {msg_confirm_from_server, MsgId}
end
else błąd
Serwer ->> Klient1: does_not_exist
end

```

Klient1 pyta serwer, czy dany użytkownik istnieje. Jeśli tak, to wywołuje funkcję serwera w postaci send_message(Username, Time, From, Message, MsgId). Serwer przekazuje wiadomość do Klient2 w postaci {Time, From, Message, MsgId}, otrzymuje potwierdzenie otrzymania wiadomości od Klient2 w postaci {msg_confirm_from_client, MsgId}. Klient1 otrzymuje potwierdzenie odebrania wiadomości od serwera w postaci {msg_confirm_from_server, MsgId}. Jeżeli nie ma takiego użytwnika serwer odpowiada "does_not_exist".

### users

```mermaid
sequenceDiagram
Klient ->> Serwer: show_active_users() 
Serwer ->> Klient: [ActiveUsers]
```
Klient wywołuje funkcję serwera w postaci show_active_users(). W odpowiedzi serwer wysyła [ActiveUsers].

### set_pass
```mermaid
sequenceDiagram
Klient ->> Serwer: set_password(Name, Password)  
Serwer ->> Klient: ok
```
Klient wywołuje funkcję serwera w postaci set_password(Name, Password). W odpowiedzi serwer wysyła "ok".

### history

```mermaid
sequenceDiagram
Klient ->> Serwer: sprawdzenie czy użytkownik jest zarejestrowany
alt sukces
Klient ->> Serwer: user_history(Username) 
Serwer ->> Klient: [{Time, From, Message},...]  
end
```
Klient wywołuje funkcję serwera w postaci user_history(Username). W odpowiedzi serwer wysyła listę zapisanych wiadomości w postaci [{Time, From, Message},...].


