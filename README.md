# Project description

Celem projektu stworzenie jest 2 aplikacji: klienta i serwera. Klienci łączą się z serwerem i za jego pośrednictwem wymieniają się wiadomościami.
Serwer i każdy z klientów to osobny node beam.

Klient musi posiadać powłokę umożliwiającą mu logowanie do systemu, wysyłanie i odbieranie wiadomości, pobieranie listy aktywnych użytkowników, pobieranie historycznych wiadomości, wylogowanie. Musi istnieć możliwość wysyłania wiadomości do wszystkich użytkowników lub jednego konkretnego. Powinna istnieć możliwość, aby użytkownik dla swojej nazwy mógł założyć hasło, tak, aby w przyszłości logowanie mogło odbyć się tylko po jego podaniu. Klient powinien potwierdzać otrzymanie wiadomości od serwera. Klient może posiadać dodatkowy graficzny interfejs użytkownika.

Serwer musi przekazywać komunikaty między użytkownikami, aktywności serwera powinny być logowane do pliku. Serwer powinien być konfigurowalny. Konfiguracja powinna obejmować lokalizację pliku z logiem i dozwoloną liczbę maksymalnie podłączonych użytkowników. Serwer powinien przesłać do klienta adresata wiadomość od razu po jej otrzymaniu. Serwer powinien przesyłać do klienta nadawcy informację, że jego wiadomość została odebrana przez klienta adresata. Serwer powinien po ponownym połączeniu użytkownika wysłać wszystkie wiadomości do niego, które pojawiły się, gdy nie był zalogowany. Serwer musi automatycznie wylogować użytkownika w momencie wykrycia braku połączenia z klientem po najpóźniej 2 minutach. Serwer powinien zapisywać zawartość komunikacji między klientami w sposób trwały między uruchomieniami.

Wszystkie przesyłane ciągi znakowe (np. nazwy użytkowników, wiadomości) muszą zawierać tylko znaki drukowane ASCII i powinny być przesyłane w kodowaniu 7 bitowym.

Cały system musi być przetestowany za pomocą testów jednostkowych z użyciem EUnit. Wszystkie scenariusze komunikacji klient - serwer powinny być zaimplementowane jako testy.

Zespół musi dostarczyć pełną dokumentację protokołu komunikacji między klientem i serwerem. Zespół musi dostarczyć instrukcję działania systemu, instrukcję budowania i uruchomienia. Wszystkie instrukcje powinny znajdować się w repozytorium w formacie MD.

Zespół musi dostarczyć wysoko poziomowy projekt systemu w podziale na logiczne bloki i powiązania między nimi.

Na koniec każdego przyrostu zespół musi dostarczyć działającą wersję aplikacji i spójną z nią dokumentację. Wersja musi być stabilna tj. wszystkie funkcjonalności dostarczone w tej i wcześniejszych wersjach aplikacji muszą działać prawidłowo. Zespół co tydzień będzie wskazywał rewizję kodu z głównej gałęzi repozytorium, która zawiera tę wersję. Na głównej gałęzi repozytorium mogą znajdować się tylko stabilne wersje.

