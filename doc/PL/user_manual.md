
# Instrukcja użytkownika

Niniejszy dokument zawiera instrukcję użytkownika dla klienta komunikatora internetowego. Format wiadomości zależny od języka, który został wybrany przez użytkownika (PL/EN).
> **Uwaga:** Jedyne dozwolone znaki to znaki drukowane ASCII.

## Opis interfejsu użytkownika

Przy uruchomieniu programu za pomocą skryptu, pojawia się komunikat:

```erlang
Select your language
pl -polski
en -english
Option:
```

Należy podać odpowiedni skrót języka, który użytkownik chce wybrać. 

Po uruchomieniu programu pojawi się komunikat:

```
Witaj w komunikatorze Erlangpol!
Prosze wpisz swoja nazwe uzytkownika:
``` 


Po którym należy podać nazwę użytkownika. 
Po wpisaniu nazwy użytkownika, klient wyświetla Wiadomość Dnia. Domyślna Wiadomość dnia jest następująca:

```
!!!Erlangpol server: In this server we slay our enemies!!!
```

Jeśli nazwa jest zabezpieczona hasłem, pojawi się komunikat:

```
Ten uzytkownik jest chroniony haslem.
Prosze wpisz swoje haslo:
```

Serwer sprawdzi, czy osiągnięta została maksymalna liczba zalogowanych użytkowników. Jeśli tak, pojawi się komunikat:

```
Maksymalna liczba uzytkownikow zalogowanych zostala osiagnieta.
```

Jeśli nie, to serwer sprawdzi, czy użytkownik o tej nazwie nie jest aktualnie zalogowany. Jeśli jest, pojawi się komunikat:

```
Uzytkownik o takiej nazwie juz istnieje
```

Jeśli nie, to w przypadku gdy wymagane było podanie hasła, serwer sprawdzi jego poprawność.

W przypadku gdy podano złe hasło, pojawi się komunikat:

```
Haslo niepoprawne, prosze sprobowac ponownie
```

W przypadku podania poprawnego hasła lub gdy nie było ono wymagane, nastąpi zalogowanie.  
Po zalogowaniu pojawi się komunikat

```
Polaczono z serwerem
Aby wyswietlic dostepne komendy, wpisz "help"
```
oraz znak zachęty:

```
@Username>
```

W przypadku poprawnego połączenia, można wpisać polecenie `help`. Pojawi się wtedy komunikat:

```
Mozesz skorzystac z nastepujacych komend:
logout                  by wylogowac sie z serwera
send                    by wyslac wiadomosc do wszystkich uzytkownikow
send Username           by wyslac wiadomosc do uzytkownika z konkretna nazwa 
users                   by wyswietlic liste aktywnych uzytkownikow
set_pass                by zalozyc nowe haslo
history                 by wyswietlic historie wiadomosci (tylko dla zalogowanych uzytkownikow)
help                    by wyswietlic te liste komend ponownie 
exit                    by wyjsc z aplikacji
```

Po wybraniu opcji `send`  pojawi się znak zachęty:

```
Wiadomosc>
```
Po wpisaniu dowolnego dozwolonego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który prześle ją do wszystkich zalogowanych użytkowników. Pojawi się wtedy również komunikat:

```
Wyslales wiadomosc do wszystkich uzytkownikow
```

Po wybraniu opcji `send <Username>`  pojawi się znak zachęty:

```
Wiadomosc>
```

Po wpisaniu dowolnego dozwolonego ciągu znaków i naciśnięciu `enter` wiadomość zostanie przekazana do serwera, który sprawdzi, czy dany użytkownik <Username> istnieje w systemie. Jeśli tak, to prześle ją do serwera. Pojawi się wtedy komunikat:

```
Wyslales wiadomosc do <Username>
```
  
W przypadku, gdy użytkownik <Username> istnieje i jest aktualnie zalogowany, serwer prześle do niego wiadomość. Jeśli użytkownik <Username> nie jest zalogowany, ale jest zarejestrowany (zabezpieczony hasłem), wysłana wiadomość zostanie zapisana do jego mailboxa i wysłana po zalogowaniu.
  
W przypadku, gdy użytkownik nie istnieje w systemie, pojawi się komunikat:
  
```
Nie ma takiego uzytkownika!
```

> **Uwaga:** W programie został zaimplementowany bufor. Oznacza to, że terminal jest zablokowany, dopóki użytkownik nie wyśle wiadomości. Oznacza to też, że użytkownik nie dostanie w tym czasie żadnej wiadomości przesłanych przez innych użytkowników.

Po wybraniu opcji `users`  wyświetlona zostanie lista aktualnie zalogowanych użytkowników:

```
Lista aktywnych uzytkownikow: [<Username1>, <Username2>, ...]
```
  
Po wybraniu opcji `set_pass`  pojawi się komunikat:

```
Prosze wpisz haslo:
```
  
Po którym należy podać hasło, które chcemy ustawić. Po wpisaniu dowolnego ciągu znaków i naciśnięciu `enter` hasło zostanie przekazane do serwera, który przypisze je do aktualnie zalogowanego użytkownika. Od tej pory przy logowaniu wymagane będzie podanie przypisanego hasła. Pojawi się wtedy również komunikat:
  
```
Haslo zostalo ustawione
```

Po wybraniu opcji `history`, w przypadku użytkownika niezarejestrowanego, pojawi się komunikat:

```
Tylko zarejestrowani uzytkownicy maja dostep do historii wiadomosci
```

w przypadku użytkownika zarejestrowanego, wyświetlona zostanie historia wiadomości. Każda wiadomość wyświetli się w formacie:

```
Year/Month/Day Time - From: Message
```

lub, jeśli skrzynka odbiorcza jest pusta, pojawi się komunikat:

```
Twoja historia wiadomosci jest pusta
```

Po wybraniu opcji `logout` klient rozłączy się z serwerem. Pojawi się komunikat:

```
Zostales wylogowany
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



