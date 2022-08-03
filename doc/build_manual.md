# Instrukcja budowania i uruchamiania


Niniejszy dokument zawiera instrukcję budowania i uruchamiania systemu:

## Instrukcja budowania


Do budowania system wykorzystuje...

Aby zbudować system należy wykonać nnastępujące polecenie w wierszu poleceń:

rebar3 do clean, compile

## Instrukcja uruchamiania


Aby uruchomić aplikację wykonać następujące polecenie w wierszu poleceń:
rebar3 shell

Aby uruchomić serwer należy wykonać następujące polecenie w wierszu poleceń:

erl -bl elks -a server


Aby uruchomić klienta należy wykonać następujące polecenie w wierszu poleceń:

erl -bl elda -a client


## Instrukcja uruchamiania testów
  

Aby uruchomić testy należy postępować następująco:

1. Uruchomić serwer

2. Uruchomić środowisko erlanga poleceń: 

erl 

3. Wykonać polecenie: 

rebar3 do clean, compile, eunit --dir=test, cover --verbose
