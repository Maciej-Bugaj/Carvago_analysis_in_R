Wspomaganie procesu podejmowania decyzji o wyborze samochodu premium
================
Maciej Bugaj

# PRZYGOTOWANIE DANYCH

Analizie podlegać będą oferty sprzedaży samochodów należące do klasy
Premium, których to cena zlokalizowana jest w przedziale od 100 000 do
200 000 zł. Celem zrealizowania zadania wykorzystane zostaną dane
pochodzące ze strony <https://carvago.com/pl/samochody>. Do wydobycia
danych ze strony internetowej zastosowane zostaną techniki Web
Scrapingu, które w zautomatyzowany sposób pozwolą na wyodrębnienie
szeregu danych ze stron internetowych w strukturyzowanej formie.

Planowanym rezultatem projektu jest odpowiedzenie na pytania dotyczące
m.in.: poziomu wyposażenia samochodów, liczebności ofert w danych
krajach, dominacji marek pojazdów, wpływu rodzaju paliwa na emisję CO2,
zależności roku produkcji od średniej ceny auta oraz ustalenie rankingu
samochodów o najniższym spalaniu na 100km.

## Wczytanie potrzebnych pakietów

Celem korzystania z funkcji oferowanych przez R konieczne jest
zainstalowanie kilku pakietów, których funkcje będą niezbędne do
realizacji projektu.

``` r
# Wczytywanie grupy pakietów
library(tidyverse)
library(stringi)
library(rvest)
library(httr)
```

## Zdefiniowanie zmiennej user_agent

Podczas automatycznego pobierania danych ze stron www należy
“przedstawić się”, aby poinformować serwer, z którego będziemy pobierać
dane, kim jesteśmy. Informacje, które zostały przypisane do zmiennej
user zostały skopiowane ze strony <https://www.whatismybrowser.com>. Są
to informacje dotyczące naszej przeglądarki.

``` r
#przedstawienie się
user <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
```

## Wczytywanie adresów url do zmiennej strony

W projekcie zostaną pobrane dane z pierwszej strony. Ten mały zbiór
danych (20 ogłoszeń) wykorzystamy do stworzenia ramki danych. Gdy
wszystko będzie sprawnie działać, pobiorę większą ilość ogłoszeń, aby
później przeanalizować uzyskane dane.

``` r
#calosc <- list()
#oferty_strona <- list()
#oferty_all <- list()

#url <- 'https://carvago.com/pl/samochody?price-from=100000&price-to=200000&source-created-at=7&page='
#ilosc_stron <- 150
#strony <- paste0(url, 1:ilosc_stron)
```

Na początku zostały stworzone trzy puste listy. Ich zastosowanie jest
następujące: - calosc (lista, w której będą dodawane wiersze pobrane z
każdej iteracji) - oferty_strona (lista, w której będą gromadzone linki
do ogłoszeń z każdej strony - każda strona zawiera 20 ogłoszeń) -
oferty_all (lista, w której gromadzone będą wszystkie linki do ogłoszeń
np. dla wczytanych 3 stron w tej liście będzie się znajdować 3x20=60
linków do ogłoszeń)

Do zmiennej url został przypisany adres url kończący się frazą “page=”
co pozwoli na zautomatyzowanie procesu pobierania danych ze stron.
Zmienna ilosc_stron informuje o tym, z ilu stron zostaną pobrane dane. W
zmiennej strony jest łączony adres url z zdefiniowaną wcześniej ilością
stron, dzięki czemu uzyskuje dostęp do każdej strony. W tym przypadku
strony od 1 do 50 włącznie.

# AUTOMATYZACJA PROCESU

Aby pobrać dużą ilość danych należy zautomatyzować proces pobierania
danych. Do automatyzacji procesu została wykorzystana pętla for, która
wykonuje tyle iteracji ile wynosi długość zmiennej ilosc_stron. Ze
względu na czasochłonność procesu czynności dotyczące pobierania danych
ze strony zostały poprzedzone komentarzem celem uniemożliwienia
kompilacji kodu (część ta została wykonana w pliku scraper.R)

``` r
# Definiowanie zakresu stron (parametr i = 1:N)
#for(i in 1:ilosc_stron){
#  ogloszenia <- tryCatch(GET(strony[i],user_agent(user), error = function(e){
#    print(paste0("Błąd na stronie: ", i, " | Czas:", format(Sys.time(), "%x")))
#    NA
#  }))
#  linki <- content(ogloszenia)
#  linki <- linki %>% html_elements('.css-qyyoh2.e1oahio85') %>%
#    html_attr('href')
#  linki_www <- paste0("https://www.carvago.com", linki)
#  oferty_all[[i]] <- linki_www
#  Sys.sleep(2)
#  czas <- format(Sys.time(), "%X")#format(Sys.time(), "%d %B %Y %A %X")
#  print(paste0("Iteracja: ", i, " | Czas: ", czas, " | Status:", ogloszenia$status_code))
#}
```

Do zmiennej ogłoszenia została przypisana metoda GET służąca do
wysyłania żądania na serwer wraz z obsługą błędów. W zmiennej linki
zapisałem zawartość zmiennej ogłoszenia, a następnie przypisałem do niej
główną częśc strony, która zawiera wszystkie dane, które zamierzam
pobrać. W zmiennej linki_www są gromadzone wszystkie linki do ogłoszeń.
Przy każdej iteracji do listy oferty_all są dodawane linki. Proces
pobierania danych został opóźniony o 2 sekundy. Oznacza to, że po
zakończeniu każdej iteracji system odczeka dwie sekundy przed wykonaniem
następnej iteracji. Przy każdej iteracji jest wyświetlana informacja o
numerze wykonywanej iteracji, aktualnym czasie oraz o kodzie statusu
serwera.

Efekt działania pętli widoczny jest poniżej

``` r
#Gotowy zbiór wyodrębnionych linków do ofert
#oferty_all <- unlist(oferty_all)
#oferty_all
```

## Pobieranie danych ze stron

Po uzyskaniu listy zawierającej wszystkie linki do ogłoszeń możemy
przystąpić do pobierania danych z każdej z tych stron. Do wykonania tej
operacji zastosowałem koleją pętlę for, która pobiera dane z każdego
elementu listy oferty_all, czyli każdego linku. W tym przypadku została
ponownie zastosowana metoda GET wraz z obsługą błędów. Opóźnienie w
pobieraniu danych wynosi tutaj sekundę.

``` r
#Ekstrakcja danych ze strony za pomocą pętli for (parametr i = 1:N)
#for(i in 1:length(oferty_all)){
#  car <- read_html(oferty_all[i])
#  
#  ogloszenia_linki <- tryCatch(GET(oferty_all[i],user_agent(user), error = function(e){
#    print(paste0("Błąd na stronie: ", i, " | Czas:", format(Sys.time(), "%x")))
#    NA
#  }))
#  
# Sys.sleep(1)
#czas_oferty <- format(Sys.time(), "%X")#format(Sys.time(), "%d %B %Y %A %X")
#  print(paste0("Iteracja: ", i, " | Czas: ", czas_oferty, " | Status:", ogloszenia_linki$status_code))
```

Proces pobierania danych ze strony rozpocząłem od pobrania zawartości
całej strony do zmiennej Car_Info_all.

``` r
#Pobranie zawartości całej strony
#    Car_Info_all <- car %>% html_element('.ejie6uw1')
```

Pobieram informację o nazwie ogłoszenia

``` r
#Nazwa ogłoszenia 
#    Car_nazwa <- Car_Info_all %>% html_element('h1') %>% html_text2()
#    Car_nazwa <- unlist(Car_nazwa)
```

Pobieram informację o cenie brutto samochodu. Dane zostały od razu
przygotowane do dalszej analizy. Usunąłem “zł” oraz puste znaki, aby
umożliwić zmianę typu zmiennej na numeryczny.

``` r
#Cena brutto oferty zakupu auta
#    Car_cena <- Car_Info_all %>% html_element('.e1hgzarh2') %>%
#      html_text() %>%
#      stri_replace_all("", fixed = "zł")
#      # Usunięcie pustego znaku który oddziela tysiące od setek w kolumnie cena
#      Car_cena <-  stri_replace_all_charclass(Car_cena, "\\p{WHITE_SPACE}", "")
#      Car_cena <- as.numeric(Car_cena)
```

Pobieram informację o kraju z jakiego pochodzi samochód

``` r
#Kraj pochodzenia samochodu    
#    Car_Kraj <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
#    section[1]/div[2]/div[2]/div[3]/div[2]/div[2]/div[2]') %>%
#     html_text()
```

Pobieram informację o tym kim jest sprzedawca (Dealer / Komis)

``` r
#Kto jest sprzedawcą
#    Car_Typ_Sprzedawcy <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
#    section[1]/div[2]/div[2]/div[3]/div[2]/div[1]/div[2]/text()') %>%
#      html_text()
```

Pobieram informację o ID ogłoszenia, co pozwoli zweryfikować czy któreś
ogłoszenie nie jest zdublowane

``` r
#Identyfikator oferty
#    Car_ID_Oferty_Auta <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
#    section[1]/div[2]/div[1]/div[1]/div[2]/span') %>%
#      html_text()
```

Pobieram informację o liczbie zdjęć, które zawiera ogłoszenie

``` r
#Liczba zdjęć jaka została dodana do oferty
#   Car_Liczba_zdjec <- Car_Info_all %>% html_element('.euepjw11') %>%
#      html_text()
#    Car_Liczba_zdjec <- as.numeric(Car_Liczba_zdjec)
```

Pobieram informację o marce samochodu

``` r
#Marka pojazdu
#    Car_marka <- Car_Info_all %>% html_element('.ey3doxd4') %>%
#      html_text()
```

Pobieram informację o modelu samochodu

``` r
#Model pojazdu
#    Car_model <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[1]/div/div[2]/div[2]/div[2]') %>%
#      html_text()
```

Pobieram informację o mocy silnika samochodu (moc jest wyrażana w kW)

``` r
#Moc silnika 
#    Car_moc <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[1]/div[2]') %>%
#      html_text() %>%
#      stri_replace_all("", fixed = " kW")
```

Pobieram informację o rodzaju napędu samochodu

``` r
#Rodzaj napędu
#    Car_naped <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#    section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[2]/div[2]') %>%
#      html_text()
```

Pobieram informację o rodzaju skrzyni biegów

``` r
#Rodzaj skrzyni biegów
#    Car_skrzynia_biegow <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#    section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[3]/div[2]') %>%
#      html_text()
```

Pobieram informację o pojemności silnika (wyrażona w jednostce CC -
Cubic Centimeter)

``` r
#Pojemność silnika wyrażona w jednostce CC - Cubic Centimeter
#    Car_pojemnosc_silnika <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[4]') %>%
#      html_text() %>%
#      stri_replace_all("", fixed = "Pojemność silnika") %>%
#      stri_replace_all("", fixed = " cc")
#      Car_pojemnosc_silnika <- stri_replace_all_charclass(Car_pojemnosc_silnika,"\\p{WHITE_SPACE}","")
```

Pobieram informację o paliwie, jakim napędzany jest samochód

``` r
#Rodzaj paliwa 
#    Car_paliwo <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[5]/div[2]') %>%
#      html_text()
```

Pobieram informację o średnim zużyciu paliwa na 100km. Aby uzyskać samą
wartość numeryczną musiałem usunąć tekst, który towarzyszył pobranej
informacji o zużyciu paliwa.

``` r
#Średnie zużycie paliwa na 100km
#    Car_zuzycie_paliwa_na_100km_srednie <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/
#      div/main/section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[6]/div[1]/div[2]') %>%
#      html_text() %>%
#      stri_replace_all("", fixed = "(śred.)") %>%
#      stri_replace_all("", fixed = "km") %>%
#      stri_replace_all("", fixed = "l/100") %>%
#      stri_trim_right()
```

Pobieram informację o poziomie emisji Co2 na każde 100km

``` r
#Poziom emisji CO2 
#    Car_emisja_CO2 <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[7]/div[2]') %>%
#      html_text() 
```

Pobieram informację o klasie emisji Co2

``` r
#Klasa emocji CO2
#    Car_klasa_emisji <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[8]/div[2]') %>%
#      html_text()
```

Pobieram informację o przebiegu samochodu (wyrażonym w km). Aby uzyskać
zmienną numeryczną musiałem usunąć końcówkę “km”.

``` r
#Przebieg pojazdu wyrażony w km
#    Car_przebieg_km <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[1]/div[2]') %>%
#      html_text() %>%
#      stri_replace_all("", fixed = " km")
#      Car_przebieg_km <-  stri_replace_all_charclass(Car_przebieg_km, "\\p{WHITE_SPACE}", "")
```

Pobieram informację o dacie pierwszej rejestracji samochodu

``` r
#Miesiąc oraz rok pierwszej rejestracji
#    Car_pierwsza_rejestracja <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[2]/div[2]') %>%
#      html_text()
```

Pobieram informację o ilości poprzednich właścicieli samochodu

``` r
#Ilu było poprzednich właścicieli
#    Car_poprzedni_wlasciciel <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
#      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[3]/div[2]') %>%
#      html_text()
```

Pobieram informacje o wyposażeniu samochodu. Ta zmienna jest
przekształcona na wektor zawierający ciąg znaków typu tekstowego
oddzielonych przecinkiem.

``` r
#Car_wyposazenie <- Car_Info_all %>% html_elements('.css-162b12t.ep0w0591') %>%
#      html_text()
#    Car_wyposazenie <- subset(Car_wyposazenie, nchar(as.character(Car_wyposazenie)) < 30)
#    Car_wyposazenie <- na.omit(Car_wyposazenie)
#    Car_wyposazenie <- as.vector(Car_wyposazenie)
#    Car_wyposazenie <- paste(Car_wyposazenie, sep="", collapse =",")
```

Pobieram informacje o ilości elementów wyposażenia znajdujących się w
samochodzie. Do uzyskania rzeczywistej ilości elementów wyposażenia
zostały policzone przecinki oraz dodana wartość 1, ponieważ pierwszy
element z wektora nie jest poprzedzony przecinkiem.

``` r
#Ilosc_elementow <- stri_count(Car_wyposazenie, regex = ",") + 1
```

## Stworzenie ramki danych

Po przypisaniu wszystkich danych do zmiennych przechowujących
poszczególne informacje można stworzyć ramkę danych zawierającą
wszystkie wyżej wymienione informacje o samochodach. Od razu przypisałem
także nazwy kolumn. Ramka zawiera jeden wiersz dla każdej iteracji, więc
aby stworzyć ramkę danych zawierającą informacje o wszystkich ofertach
postanowiłem do listy calosc dodać każdy wiersz z rd_cars czyli z ramki
danych. Dopiero na tym etapie można zamknąć pętlę for, która została
omówiona w punkcie 2.1.

``` r
# RAMKA DANYCH
#    rd_cars <- tibble(Nazwa_Oferty = Car_nazwa, ID_Oferta = Car_ID_Oferty_Auta,
#                      Marka = Car_marka, Model = Car_model, Cena_Brutto = Car_cena,
#                      Rodzaj_Napedu = Car_naped, Typ_Skrzynia = Car_skrzynia_biegow,
#                      Pojemnosc_Silnika = Car_pojemnosc_silnika, Typ_Paliwa = Car_paliwo,
#                      Zuzycie_Paliwa_na_100km = Car_zuzycie_paliwa_na_100km_srednie,
#                      Emisja_CO2 = Car_emisja_CO2, Klasa_Emisji = Car_klasa_emisji,
#                      Przebieg_Km = Car_przebieg_km, Pierwsza_Rejestracja = Car_pierwsza_rejestracja,
#                      Poprzedni_wlasciciel = Car_poprzedni_wlasciciel, Kraj = Car_Kraj,
#                      Typ_Sprzedawcy = Car_Typ_Sprzedawcy, Moc_Silnika = Car_moc,
#                      Liczba_Zdjec = Car_Liczba_zdjec, Wyposazenie = Car_wyposazenie, 
#                      Liczba_elementow_wyposazenia = Ilosc_elementow)
#                           
#    
#    calosc[[i]] <- rd_cars
# }
```

Gdy już mam listę wypełnioną danymi to mogę połączyć wiersze funkcją
bind_rows. Dzięki temu uzyskałem interesującą nas ramkę danych, którą
następnie zapisałem do pliku dane_www.csv.

``` r
#calosc <- bind_rows(calosc)
#head(calosc, 10)
#write.csv(calosc, "dane_www.csv")
```

Po zakończeniu procesu pozyskiwania danych i zapisywania ich w ramce
danych mogę przystąpić do analizy danych.

# ANALIZA DANYCH

Ramka, którą udało mi się stworzyć zawiera prawie 3000 wierszy. Niektóre
dane były niepełne lub niewłaściwe, przez co nie zostawały dodawane do
ramki.

## Wczytanie danych

Do wczytania ramki danych, którą utworzyłem w poprzednim rozdziale
użyliśmy funkcji read.csv z argumentem sep = “,”, ponieważ kulumny w
pliku csv są oddzielone przecinkami. Podczas exportu ramki do pliku csv
utworzyła się niechciana kolumna o nazwie X, dlatego po zaimportowaniu
danych usunąłem ją.

``` r
df_cars <- read.csv("dane_www.csv", sep = ',')
df_cars$X <- NULL

#Wywołanie finalnej ramki danych (po zastosowaniu pętli for) dla 2 wierszy
df_head_5 <- head(df_cars, 2)
kable(df_head_5[, 1:8])
```

| Nazwa_Oferty                          | ID_Oferta | Marka    | Model | Cena_Brutto | Rodzaj_Napedu | Typ_Skrzynia | Pojemnosc_Silnika |
|:--------------------------------------|----------:|:---------|:------|------------:|:--------------|:-------------|------------------:|
| Cadillac ATS AWD AT8 203 kW           |  48276303 | Cadillac | ATS   |      133649 | 4x4           | Automat      |              1998 |
| Seat Leon SC 2.0 TSI Cupra 290 213 kW |  48311169 | Seat     | Leon  |      148649 | 4x2           | Automat      |              1984 |

``` r
#Ze względu na dużą ilość kolumn (zmiennych) pierwotna ramka danych została podzielona na trzy części
kable(df_head_5[ , 9:15])
```

| Typ_Paliwa | Zuzycie_Paliwa_na_100km | Emisja_CO2 | Klasa_Emisji | Przebieg_Km | Pierwsza_Rejestracja | Poprzedni_wlasciciel |
|:-----------|:------------------------|-----------:|:-------------|------------:|:---------------------|:---------------------|
| Benzyna    | 8.6                     |        203 | Euro 6       |       29800 | 12/2018              | 2                    |
| Benzyna    | 6.6                     |        154 | Euro 6d-TEMP |       23650 | 5/2020               | 1                    |

``` r
#Ostatni fragment ramki bazowej
kable(df_head_5[ , 16:21])
```

| Kraj   | Typ_Sprzedawcy | Moc_Silnika | Liczba_Zdjec | Wyposazenie                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | Liczba_elementow_wyposazenia |
|:-------|:---------------|:------------|-------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------:|
| Niemcy | Komis          | 203         |           15 | ABS,ESP,Alarm,Asystent hamowania (EBA, BAS),System start-stop,Czujnik martwego pola,Wspomaganie kierownicy,Asystent pasa jazdy,Czujnik ciśnienia w oponach,Kamera parkowania,Czujniki parkowania przód/tył,USB,Ekran dotykowy,Komputer pokładowy,Radio samochodowe,Podłokietnik z przodu,Skórzana kierownica,Kierownica wielofunkcyjna,Tempomat,Felgi aluminiowe,Reflektory adaptacyjne,Czujnik zmierzchu,Światła do jazdy dziennej,Zamek centralny,Czujnik wycieraczek,Reflektory ksenonowe                                   |                           27 |
| Niemcy | Dealer         | 213         |            6 | ABS,ESP,System ochrony pieszych,Funkcja Auto Hold,Asystent hamowania (EBA, BAS),System start-stop,Wspomaganie kierownicy,Immobiliser,Asystent pasa jazdy,Czujnik ciśnienia w oponach,Kamera parkowania,Radio,USB,Komputer pokładowy,Radio samochodowe,Felgi aluminiowe,Automatyczne światła drogowe,Czujnik zmierzchu,Światła do jazdy dziennej,Tylna wycieraczka,Zamek centralny,Czujnik wycieraczek,Oświetlenie ambientowe,Podłokietnik z przodu,Skórzana kierownica,Kierownica wielofunkcyjna,ISOFIX na tylnych siedzeniach |                           28 |

W kolumnie poprzedni_właściciel znajdowały się wartości numeryczne oraz
tekstowe, dlatego wyciągnąłem liczbę z każdego wiersza, a wartości nie
będące liczbami zamieniliśmy na wartości NA.

``` r
df_cars$Poprzedni_wlasciciel <- df_cars$Poprzedni_wlasciciel %>% 
  stri_extract_all(regex = "\\d") %>%
  stri_sub(from = 1, to = 2)%>%
  stri_replace(NA, fixed = "c(")
head(df_cars$Poprzedni_wlasciciel, 10)
```

     [1] "2" "1" "1" "1" NA  "1" NA  "1" NA  "1"

Poniższa tabela pokazuje ilu poprzednich właścicieli miał samochód zanim
ogłoszenie o jego sprzedaży trafiło na stronę. Wartość NA oznacza, że
ogłoszeniodawca nie umieścił tej informacji w ogłoszeniu lub, że
samochód nie posiadał żadnego właściciela przed sprzedażą.

``` r
#Ilu poprzednich właścicieli posiadało samochód?
df_wlasciciel <- tibble(Ilosc_aut = df_cars$Poprzedni_wlasciciel)
wlasciciele <- df_wlasciciel %>% group_by(Ilosc_aut)%>%
  count()
head(wlasciciele)
```

    # A tibble: 6 x 2
    # Groups:   Ilosc_aut [6]
      Ilosc_aut     n
      <chr>     <int>
    1 1          1347
    2 2           317
    3 3            10
    4 4             3
    5 5             1
    6 <NA>       1219

## Analiza wyposażenia samochodów

Poniższy kod przedstawia analizę elementów wyposażenia samochodów. Aby
przetworzyć te dane, które są typu teksowego, musiałem podzielić je
ustawiając parametr fixed na przecinek. Następnie wyciągnąłem unikalne
elementy wyposażenia, aby dowiedzieć się jaki odsetek samochodów premium
posiada dany element wyposażenia. Efektem tej analizy jest wykres
przedstawiający odsetek samochodów posiadających dany element
wyposażenia.

``` r
#WYPOSAZENIE--------------------------------------------------------------------
wyp <- stri_split(df_cars$Wyposazenie, fixed = ",")
wypUniq <- unique(unlist(wyp))

nazwaWyp <- wypUniq %>%
  stri_trans_tolower() %>%
  stri_trans_general("Latin-ASCII") %>%
  stri_replace("", fixed = "(") %>%
  stri_replace("", fixed = ")")

wyposazEtykiety <- data.frame(wyposaz = nazwaWyp, wyposazEtykieta = wypUniq)
wyposazEtykiety <- drop_na(wyposazEtykiety)

nalezy <- map(wyp, ~1*(wypUniq %in% .x))
dfWyp <- do.call(rbind, nalezy)
dfWyp <- data.frame(dfWyp)

tabWyposaz <- dfWyp %>% summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(),
               names_to = "wyposaz",
               values_to = "proc_wyposaz") %>%
  arrange(-proc_wyposaz)

tabWyposaz <- cbind(wyposazEtykiety$wyposaz, tabWyposaz$proc_wyposaz)
tabWyposaz <- data.frame(tabWyposaz)
names(tabWyposaz) <- c("Wyposazenie", "odsetek_samochodow")
tabWyposaz$odsetek_samochodow <- as.numeric(tabWyposaz$odsetek_samochodow)


ggplot(tabWyposaz, aes(reorder(Wyposazenie, odsetek_samochodow), odsetek_samochodow)) + theme_bw()+
  geom_col(fill = "#4C82FC") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,1,0.05),
                     expand = expansion(mult = c(0,0), add = c(0,0.05))) +
  labs(x = NULL, y = NULL)
```

<img src="carvago_files/figure-gfm/unnamed-chunk-34-1.png" style="display: block; margin: auto;" />
Z powyższego wykresu można odczytać, że ponad 90% samochodów premium
posiada takie elementy wyposażenia jak: ABS, ESP, Alarm oraz system
asystenta hamowania. Co więcej można wspomnieć, iż wymienione elementy
stanowią podstawowe wyposażenie jakie powinien zawierać każdy samochód
celem zachowania bezpieczeństwa podróży.

## Top 5 krajów pod względem liczby samochodów

Krajem, z którego pochodzi aż 78% samochodów są Niemcy, na drugiej
pozycji znajduje się Francja i stanowi ona około 17% z wszystkich
samochodów w naszej ramce danych.

``` r
#liczba ofert z każdego kraju
df_kraj <- tibble(kraj = df_cars$Kraj)
kraj <- df_kraj %>% group_by(kraj)%>%
  count()%>%
  arrange(desc(n))
head(kraj)
```

    # A tibble: 6 x 2
    # Groups:   kraj [6]
      kraj          n
      <chr>     <int>
    1 Niemcy     2269
    2 Francja     492
    3 Hiszpania    53
    4 Belgia       31
    5 Austria      15
    6 Holandia     12

Jak się okazuje Niemcy są bezprecedensowym liderem w produkcji
samochodów oraz ich dalszej sprzedaży czego najlepszym potwierdzeniem
jest utworzone powyżej zestawienie. Najwięcej samochodów w Niemczech
produkuje Grupa Volkswagen, która to odpowiada za takie marki jak: Audi,
Volkswagen, SEAT, Skoda, Lamborghini czy Porsche.

## Dominujące marki w ofertach sprzedaży aut premium

Następny wykres prezentuje marki, których jest najwięcej w analizowanym
zbiorze danych.

``` r
#Dominujące marki w ofertach sprzedaży aut premium
df_marka <- tibble(marka = df_cars$Marka)
marka <- df_marka %>% group_by(marka)%>%
  count()%>%
  arrange(desc(n))

ggplot(marka, aes(x = n, y = reorder(marka, n)))+ 
  geom_col(size=3, fill = "#4C82FC")+
  labs(title = "Dominujące marki w ofertach sprzedaży aut premium")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(y= "Marka", x = "")
```

<img src="carvago_files/figure-gfm/unnamed-chunk-36-1.png" style="display: block; margin: auto;" />
Powyżej zaprezentowany wykres jest jedynie potwierdzeniem wcześniej
teorii na temat przewagi Niemiec w produkcji (i sprzedaży) samochodów
marki Premium. Zauważyć można, iż znaczącą większość marek stanowią
takie jednostki jak: BMW, Volkswagen, Mercedes-Benz, Audi czy SEAT.

## Wpływ rodzaju paliwa na emisję CO2 (g/100km)

Poniższy wykres informuje o średnim poziomie emisji Co2 (g/100km) w
zależności od rodzaju paliwa. Gaz (LPG), benzyna oraz Diesel wytwarzają
najwięcej Co2. Najmniej dwutlenku węgla emitują samochody hybrydowe oraz
CNG.

``` r
#Wpływ rodzaju paliwa na emisję CO2 (g/100km)
df_paliwo <- df_cars %>% group_by(Typ_Paliwa)
df_CO2 <- df_paliwo %>%
  summarise(Emisja_CO2= round(mean(Emisja_CO2),0))

ggplot(df_CO2, aes(x = Typ_Paliwa , y = Emisja_CO2))+
  geom_col( fill = "#4C82FC")+
  labs(title = "Wpływ rodzaju paliwa na emisję CO2")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(y= "Poziom emisji CO2 (g/100km)", x = "")
```

<img src="carvago_files/figure-gfm/unnamed-chunk-37-1.png" style="display: block; margin: auto;" />
Jak można zaobserwować największy wpływ na emisję CO2 ma LPG oraz
benzyna, natomiast najniższy negatywny wpływ mają paliwa stosowane do
samochodów hybrydowych których to wskaźnik emisji jest niemal o połowę
niższy w stosunku do uprzednio wymienionych.

## Wpływ roku produkcji auta na jego cenę

Tworząc poniższy wykres chciałem uzyskać odpowiedź czy rok produkcji
samochodów premium ma duży wpływ na ich cenę. Okazało się, że
niekoniecznie. Samochody z roku 2013 kosztują około 10 000zł mniej niż
samochody wyprodukowane w bieżącym roku.

``` r
#Wpływ roku produkcji auta na jego cenę
df_cars$Rok_produkcji <- df_cars$Pierwsza_Rejestracja %>%
stri_extract_all(regex = "(\\d\\d\\d\\d)") %>%
  as.numeric()

df_rok <- df_cars %>% group_by(Rok_produkcji)
df_cena <- df_rok %>%
  summarise(Cena = mean(Cena_Brutto))%>%
  arrange(desc(Rok_produkcji))

ggplot(df_cena, aes(x = Rok_produkcji, y = Cena))+ 
  geom_col(size=3, fill = "#4C82FC")+
  labs(title = "Średnia cena auta w zależności od roku produkcji")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(y= "Marka", x = "") +
  scale_x_continuous(breaks = seq(2010,2023,1))
```

<img src="carvago_files/figure-gfm/unnamed-chunk-38-1.png" style="display: block; margin: auto;" />
Powyższy wykres może być bardzo istotną informacją zarówno dla nabywcy
jak i sprzedawcy auta. Zauważyć można, iż największy spadek cenowy tyczy
się pojazdów wyprodukowanych 7 lat temu a ich wartość może być niższa o
prawie 40 tys. zł.

Należy wziąć jednak pod uwagę także ile jest samochodów z każdego roku,
aby mieć szerszy pogląd na powyższy wykres.

``` r
df_ile_rocznik <- df_rok %>%
  summarise(Ilosc_aut = length(Rok_produkcji))%>%
  arrange(desc(Ilosc_aut))
head(df_ile_rocznik, 15)
```

    # A tibble: 11 x 2
       Rok_produkcji Ilosc_aut
               <dbl>     <int>
     1          2021       854
     2          2018       628
     3          2020       591
     4          2019       463
     5          2017       253
     6          2016        49
     7          2022        28
     8          2015        17
     9          2014         6
    10          2012         5
    11          2013         3

Jak widać z powyższej tabeli, samochodów z roku 2013 są tylko 3, a więc
nie należy brać pod uwagę analizując powyższy wykres. Najwięcej
samochodów jest z lat 2021, 2018, 2020 oraz 2019

## Marki z najdroższymi samochodami

Następny wykres przedstawia zestawienie marek samochodów premium,
których średnia cena jest najwyższa. Marki takie jak: - Cupra - Subaru -
Volvo - Porshe - Mercedes-Benz produkują najdroższe samochody spośród
wszystkich widocznych na wykresie. Średnia cena samochodu marki Cupra
plasuje się w okolicach 175 000zł.

``` r
#Która marka ma najdroższe samochody?
df_marka2 <- df_cars %>% group_by(Marka)
df_cena2 <- df_marka2 %>%
  summarise(Cena = mean(Cena_Brutto))%>%
  arrange(desc(Cena))

ggplot(df_cena2, aes(x = Cena, y = reorder(Marka, Cena)))+ 
  geom_col(size=3, fill = "#4C82FC")+
  labs(title = "Średnia cena samochodu danej marki")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(y= "Marka", x = "")
```

<img src="carvago_files/figure-gfm/unnamed-chunk-40-1.png" style="display: block; margin: auto;" />
Do najdroższych samochodów zaliczyć można takie marki jak: Cupra,
Subaru, Volvo oraz Porsche których to średnia cena samochodu
przekroczyła 150 000 zł.

## Marki samochodów posiadające najbogatsze wyposażenie

Poniższa tabela prezentuje, marki samochodów mające najbogatsze
wyposażenie. Mówiąc najbogatsze wyposażenie, mam na myśli ilość
elementów wyposażenia, a więc im więcej elementów wyposażenia posiada
samochód tym jest lepiej wyposażony. Porshe, Subaru oraz Cupra posiadają
średnio największą ilość elementów wyposażenia.

``` r
#Jakie marki samochodów mają najbogatsze wyposażenie?
df_marka2 <- df_cars %>% group_by(Marka)
df_elementy_wyp <- df_marka2 %>%
  summarise(Elementy_wyp = round(mean(Liczba_elementow_wyposazenia)))%>%
  arrange(desc(Elementy_wyp))

head(df_elementy_wyp,10)
```

    # A tibble: 10 x 2
       Marka         Elementy_wyp
       <chr>                <dbl>
     1 Porsche                 34
     2 Subaru                  33
     3 Cupra                   31
     4 Mazda                   31
     5 Seat                    31
     6 Kia                     30
     7 Skoda                   29
     8 Ford                    28
     9 Mercedes-Benz           28
    10 Opel                    28

## Top 10 samochodów mających najmniejsze spalanie na 100km

Ostatnia tabela przedstawia ranking 10 samochodów posiadających
najniższe spalanie na 100km. Wyniki są zaskakujące, ponieważ najniższe
spalanie wynosi zaledwie 1.1 przy paliwie CNG (Compressed Natural Gas)
oraz podobne spalanie przy silniku hybrydowym. Wynika to z tego, że
silniki hybrydowe nie zużywają zbyt wiele paliwa. Pierwszy wynik jest
najwidoczniej błędem przy wprowadzaniu danych na stronie, dlatego tego
przypadku nie będziemy analizować.

``` r
# Top 10 samochodów, które mają najmniejsze spalanie na 100km
df_cars$Zuzycie_Paliwa_na_100km <- as.numeric(df_cars$Zuzycie_Paliwa_na_100km)
df_auto <- df_cars %>% group_by(Nazwa_Oferty, Typ_Paliwa)
df_spalanie <- df_auto %>%
  summarise(Spalanie = round(mean(Zuzycie_Paliwa_na_100km),2))%>%
  arrange(Spalanie)

head(df_spalanie,10)
```

    # A tibble: 10 x 3
    # Groups:   Nazwa_Oferty [9]
       Nazwa_Oferty                              Typ_Paliwa       Spalanie
       <chr>                                     <chr>               <dbl>
     1 Volvo XC40 D3 Momentum Pro 110 kW         Diesel                0  
     2 Hyundai IONIQ Trend 104 kW                CNG                   1.1
     3 Kia cee'd / Ceed 104 kW                   Hybryda               1.1
     4 Kia XCeed 104 kW                          Hybryda               1.2
     5 Renault Megane 117 kW                     Pozostałe paliwa      1.2
     6 Kia Niro 1.6 GDI 104 kW                   Hybryda               1.3
     7 Kia Niro Vision 103 kW                    Hybryda               1.3
     8 Renault Megane 117 kW                     Hybryda               1.4
     9 Seat Leon 5D 1.4 TSI DSG Xcellence 110 kW Hybryda               1.4
    10 Seat Leon SP 1.4 TSI DSG FR 110 kW        Hybryda               1.4

# WNIOSKI

Zaprezentowana powyżej analiza pozwoliła odkryć szereg zależności jakie
zachodzą pomiędzy poszczególnymi zmiennymi. Co warto podkreślić to fakt,
iż uzyskane informacje oraz wizualizacje nie są możliwe do odczytania ze
strony internetowej za pomocą dostępnych filtrów oraz sortowań.

Rezultatem projektu są zróżnicowane wizualizacje, które to w kompleksowy
sposób dostarczają użytkownikom wielu przydatnych informacji na temat
rynku samochodów Premium.

Analizując efekty projektu wyciągnąć można kilka najważniejszych
wniosków:

-   Ponad 90% wszystkich samochodów jakie zamieszczone są na serwisie
    Carvago.com to pojazdy wyposażone w system ABS, ESP oraz alarm
    bezpieczeństwa co stanowi gwarancję użytkowania samochodu oraz
    bezpieczeństwo podróży. Najrzadziej występującymi elementami
    wyposażenia jest: dach panoramiczny, asystent jazdy nocnej,
    reflektory biksenonowe oraz system śledzenia GPS.

-   Jak wykazano w części analitycznej projektu najpopularniejszymi
    lokalizacjami z których pochodzą oferty sprzedaży aut pochodzą z
    Niemiec (79%) oraz Francji (17%). Pozostałe 4% pozycji pochodzi z
    takich krajów jak Hiszpania, Belgia, Austria oraz Holandia. Należy
    zaznaczyć, iż Niemcy są zarówno europejskim jak i światowym
    potentatem w produkcji samochodów (według raportu moto.rp Niemcy
    odpowiadają za około 5,4% światowej produkcji samochodów).

-   Kolejnym wnioskiem, który jest zauważalny na pierwszy rzut oka to
    dominacja marek samochodów produkowanych w Niemczech. Mowa tutaj o
    BMW, Volkswagen, Mercedes-Benz oraz Audi, które to swoim
    asortymentem pokrywają znaczą część rynku motoryzacyjnego.

-   Następna analiza dotyczyła wpływu rodzaju paliwa na emisję CO2,
    gdzie wykazano iż najwyższy poziom średniej emisji CO2 (g/100km)
    odnotowuje LPG, którego wartość oscylowała w granicach 170 g/100km.
    Równie wysokie wskaźniki odnotowano w przypadku benzyny oraz diesel,
    gdzie wartość emisji CO2 dochodziła do nawet 140 g/100km. Jak można
    się domyślać w tym zestawieniu najlepsze rezultaty osiągnęło paliwo
    (energia) stosowane do samochodów hybrydowych oraz CNG, czyli
    sprężony gaz ziemny, których to wartości były na zbliżonym do siebie
    poziomie i wyniosły 60 g/100km.

-   Przechodząc do kolejnej analizy dotyczącej stosunku ceny auta do
    roku produkcji zauważyć można, iż samochody z roku 2022 są
    najdroższe a ich średnia cena wynosi około 170 000 zł. Zauważyć
    również można, iż największy spadek wartości pojazdu odnotowywany
    jest po 7 latach od jego wyprodukowania.

-   W kolejnym kroku poddano analizie ceny poszczególnych marek
    samochodów, którego efektem było zestawienie przedstawiające średnią
    cenę pojazdu względem salonu, w którym auto zostało wyprodukowane.
    Do najdroższych aut zaliczyć można takie marki jak: Cupra, Subaru,
    Volvo, Porsche, Mercedes-Benz oraz BMW, których to średnia cena
    przekraczała każdorazowo 150 000 zł.

-   Ostatnim wnioskiem płynącym z analiz jest zestawienie 10-ciu
    samochodów posiadających najniższe spalanie. Jak się okazuje w tym
    rankingu wygrywają samochody zasilane technologią hybrydową, które
    to tym samym są najmniej szkodliwe dla środowiska (ich emisja CO2
    jest najniższa na tle pozostałych samochodów). Powyżej zgormadzone
    informacje mogą być szczególnie istotne dla osób zamierzających
    zakupienie samochodu, które to zamierzają przeanalizować strukturę
    wyposażenia auta (oraz porównać ją z innymi modelami), określić
    poziom emisji CO2, rocznik produkcji oraz kwestie związane ze
    spalaniem.
