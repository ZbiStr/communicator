# Instrukcja budowania i uruchamiania


Niniejszy dokument zawiera instrukcję budowania i uruchamiania systemu:

## Instrukcja budowania

Do budowania system wykorzystuje rebar3 i bibliotekę OTP 24.3.4.

Aby zbudować system należy wykonać następujące polecenie w wierszu poleceń:

    rebar3 do compile

## Instrukcja uruchamiania

Aby uruchomić aplikację należy wykonać następujące polecenie w wierszu poleceń:

    rebar3 shell

Następnie, by uruchomić serwer, należy wykonać następujące polecenie w wierszu poleceń:

    communicator:start_link().

Aby uruchomić klienta należy wykonać następujące polecenie w wierszu poleceń:

    client:start().


## Instrukcja uruchamiania testów
  

Aby uruchomić testy należy wykonać następujące polecenie: 

    rebar3 do clean, compile, eunit --dir=test, cover --verbose
