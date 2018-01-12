# Projekt pod tytułem "Rozproszony silnik warcabowy"

# Autorzy:

- Szymon Sadowski, rok III, kierunek Informatyka, grupa 2b
- Krzysztof Szczyrbak, rok III, kierunek Informatyka, grupa 2b

# Data oddania: ?

# Cel programu

Celem niniejszego projektu było stworzenie silnika warcabowego, który wykorzystując mechanizmy wielowątkowości równolegle przeszukiwałby możliwe ruchy na planszy. Aby zwizualizować efekt działania silnika, postanowiliśmy również przygotować aplikację webową, która prezentuje przebieg rozgrywki. 

# Opis i schemat struktury zadaniowej programu : xD

# Informacje o stosowanych pakietach zewnętrznych (niestandardowych)

Projekt wykorzystuje pakiet Leptus (dostępny do pobrania pod adresem [https://github.com/sinasamavati/leptus](https://github.com/sinasamavati/leptus)). Pakiet Leptus posłużył nam do wystawienia REST-owego API (Programistycznego Interfejsu Aplikacji) przez sieć, tak aby można było stworzyć aplikację webową.

# Informacje o zastosowaniu specyficznych metod rozwiązania problemu: ?

# Krótka instrukcja obsługi

Aby zbudować projekt, należy przejść do folderu DistributedDraughts, a następnie wykonać poniższe polecenia:

```
./rebar3 get-deps
./rebar3 compile
```

Po zbudowaniu projektu, możemy wystartować serwer za pomocą tych poleceń:

```
./rebar3 shell
c(dra).
c(rest).
rest:server_start().
```

Dzięki temu serwer będzie nasłuchiwał pod adresem ```http://127.0.0.1:8080```

Gdy serwer wystartował, możemy przejść do katalogu client i w przeglądarce otworzyć plik ```client.html```. W ten sposób otwieramy aplikację webową.

# Testy, przykłady

# Możliwe rozszerzenia programu

- Zastosowanie algorytmu alpha-beta pruning na drzewie, dzięki czemu algorytm minimax wyznaczania najlepszego ruchu, znajdywałby ruch szybciej
- Możliwość wybrania alternatywnych zasad (jak na przykład zasady poruszania się damką: o dowolną liczbę pól/ tylko o jedno pole)
- Bardziej wyszukana funkcja oceniająca planszę

# Ograniczenia programu

- Brak możliwości generowania i przeszukiwania drzewa gry o dużej głębokości (nie zastosowano algorytmów optymalizujących jak na przykład alpha-beta pruning)
- Zbugowane damki /s

# Inne informacje zależne od tematu


