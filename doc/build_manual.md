# Instrukcja budowania i uruchamiania


Niniejszy dokument zawiera instrukcję budowania i uruchamiania systemu.  
>**Uwaga:** Wszystkie polecenia powinny być wykonywane w folderze zawierającym projekt.

## Instrukcja budowania

Do budowania system wykorzystuje rebar3 i bibliotekę OTP 24.3.4.

Aby zbudować system należy wykonać następujące polecenie w wierszu poleceń:
```erlang
rebar3 do compile
```
## Instrukcja uruchamiania

Aby uruchomić serwer, kliknij dwukrotnie plik 'run_server.sh'

Aby uruchomić klienta, należy kliknąć dwukrotnie plik 'run_clinet.sh'

>**Uwaga:** Serwer powinien być urochomiony jako pierwszy.

## Instrukcja uruchamiania testów
  
Aby uruchomić testy należy wykonać następujące polecenie: 
```erlang
rebar3 do clean, compile, eunit --dir=test, cover --verbose
```
