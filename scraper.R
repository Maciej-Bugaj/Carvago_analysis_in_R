#Tytuł projektu: "Wspomaganie procesu podejmowania decyzji o wyborze samochodu premium"
#Autor: "Maciej Bugaj"

#Wczytanie bibliotek
library(tidyverse)
library(stringi)
library(rvest)
library(httr)

####################################################################################################
#========================================carvago.com==========================================
#                     Pobieranie danych dotyczących ogłoszeń samochodowych
####################################################################################################

#przedstawienie się
user <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
# 2. Zbieranie linków do ofert =======================================================================
calosc <- list()
oferty_strona <- list()
oferty_all <- list()

url <- 'https://carvago.com/pl/samochody?price-from=100000&price-to=200000&source-created-at=7&page='
ilosc_stron <- 150
strony <- paste0(url, 1:ilosc_stron)
# Definiowanie zakresu stron (parametr i = 1:N)
for(i in 1:ilosc_stron){
  ogloszenia <- tryCatch(GET(strony[i],user_agent(user), error = function(e){
    print(paste0("Błąd na stronie: ", i, " | Czas:", format(Sys.time(), "%x")))
    NA
  }))
  linki <- content(ogloszenia)
  linki <- linki %>% html_elements('.css-qyyoh2.e1oahio85') %>%
    html_attr('href')
  linki_www <- paste0("https://www.carvago.com", linki)
  oferty_all[[i]] <- linki_www
  Sys.sleep(2)
  czas <- format(Sys.time(), "%X")#format(Sys.time(), "%d %B %Y %A %X")
  print(paste0("Iteracja: ", i, " | Czas: ", czas, " | Status:", ogloszenia$status_code))
}

#Gotowy zbiór wyodrębnionych linków do ofert
oferty_all <- unlist(oferty_all)
oferty_all

# 3. Wyciąganie informacji===========================================================================
#Ekstrakcja danych ze strony za pomocą pętli for (parametr i = 1:N)
for(i in 1:length(oferty_all)){
  car <- read_html(oferty_all[i])
  
  ogloszenia_linki <- tryCatch(GET(oferty_all[i],user_agent(user), error = function(e){
    print(paste0("Błąd na stronie: ", i, " | Czas:", format(Sys.time(), "%x")))
    NA
  }))
  
  Sys.sleep(1)
  czas_oferty <- format(Sys.time(), "%X")#format(Sys.time(), "%d %B %Y %A %X")
  print(paste0("Iteracja: ", i, " | Czas: ", czas_oferty, " | Status:", ogloszenia_linki$status_code))
  
  
    ## INFORMACJE PODSTAWOWE
    # Pobranie zawartości całej strony
    Car_Info_all <- car %>% html_element('.ejie6uw1')
    Car_Info_all
    
    #Nazwa modelu auta 
    Car_nazwa <- Car_Info_all %>% html_element('h1') %>% html_text2()
    Car_nazwa <- unlist(Car_nazwa)
    
    
    #Cena brutto oferty zakupu auta
    Car_cena <- Car_Info_all %>% html_element('.e1hgzarh2') %>%
      html_text() %>%
      stri_replace_all("", fixed = "zł")
      # Usunięcie pustego znaku który oddziela tysiące od setek w kolumnie cena
      Car_cena <-  stri_replace_all_charclass(Car_cena, "\\p{WHITE_SPACE}", "")
      Car_cena <- as.numeric(Car_cena)
    
    #Lokalizacja pochodzenia samochodu    
    Car_Kraj <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
    section[1]/div[2]/div[2]/div[3]/div[2]/div[2]/div[2]') %>%
     html_text()
    
    #Pochodzenie oferty od Dealera lub Komisu
    Car_Typ_Sprzedawcy <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
    section[1]/div[2]/div[2]/div[3]/div[2]/div[1]/div[2]/text()') %>%
      html_text()
    
    #Identyfikator oferty
    Car_ID_Oferty_Auta <- Car_Info_all %>% html_elements(xpath = '/html/body/div[1]/div/main/
    section[1]/div[2]/div[1]/div[1]/div[2]/span') %>%
      html_text()
    
    #Liczba zdjęć jaka została dodana do oferty (im więcej grafik tym większe możliwości analizy)
    Car_Liczba_zdjec <- Car_Info_all %>% html_element('.euepjw11') %>%
      html_text()
    Car_Liczba_zdjec <- as.numeric(Car_Liczba_zdjec)
      
    ## SZCZEGÓŁY SAMOCHODU
    #Marka pojazdu
    Car_marka <- Car_Info_all %>% html_element('.ey3doxd4') %>%
      html_text()
    
    #Model pojazdu
    Car_model <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[1]/div/div[2]/div[2]/div[2]') %>%
      html_text()
    
    ## PARAMETRY SILNIKA
    #Moc silnika 
    Car_moc <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[1]/div[2]') %>%
      html_text() %>%
      stri_replace_all("", fixed = " kW")

    
    #Rodzaj napędu (4x2 / 4x4)
    Car_naped <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
    section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[2]/div[2]') %>%
      html_text()
    
    #Typ skrzyni biegów (manuala / automatyczna)
    Car_skrzynia_biegow <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
    section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[3]/div[2]') %>%
      html_text()
    
    #Pojemność silnika wyrażona w jednostce CC - Cubic Centimeter
    Car_pojemnosc_silnika <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[4]') %>%
      html_text() %>%
      stri_replace_all("", fixed = "Pojemność silnika") %>%
      stri_replace_all("", fixed = " cc")
      Car_pojemnosc_silnika <- stri_replace_all_charclass(Car_pojemnosc_silnika,"\\p{WHITE_SPACE}","")
    
    #Rodzaj paliwa używanego do auta  
    Car_paliwo <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[5]/div[2]') %>%
      html_text()
    
    #Średnie zużycie paliwa na 100km
    Car_zuzycie_paliwa_na_100km_srednie <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/
      div/main/section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[6]/div[1]/div[2]') %>%
      html_text() %>%
      stri_replace_all("", fixed = "(śred.)") %>%
      stri_replace_all("", fixed = "km") %>%
      stri_replace_all("", fixed = "l/100") %>%
      stri_trim_right()
    
    #Poziom emisji CO2 
    Car_emisja_CO2 <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[7]/div[2]') %>%
      html_text() 
    
    #Klasa emocji CO2
    Car_klasa_emisji <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[2]/div/div[2]/div[8]/div[2]') %>%
      html_text()
    
    
    ## STAN SAMOCHODU
    #Przebieg pojazdu wyrażony w km
    Car_przebieg_km <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[1]/div[2]') %>%
      html_text() %>%
      stri_replace_all("", fixed = " km")
      Car_przebieg_km <-  stri_replace_all_charclass(Car_przebieg_km, "\\p{WHITE_SPACE}", "")
    
    #Miesiąc oraz rok pierwszej rejestracji
    Car_pierwsza_rejestracja <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[2]/div[2]') %>%
      html_text()
    
    #Stan pojazdu (Nowy / Używany)
    Car_poprzedni_wlasciciel <- Car_Info_all %>% html_elements(xpath = '//*[@id="__next"]/div/main/
      section[2]/div/div[1]/div[1]/div[1]/div/div/div[3]/div/div[2]/div[3]/div[2]') %>%
      html_text()
    
    Car_wyposazenie <- Car_Info_all %>% html_elements('.css-162b12t.ep0w0591') %>%
      html_text()
    Car_wyposazenie <- subset(Car_wyposazenie, nchar(as.character(Car_wyposazenie)) < 30)
    Car_wyposazenie <- na.omit(Car_wyposazenie)
    Car_wyposazenie <- as.vector(Car_wyposazenie)
    Car_wyposazenie <- paste(Car_wyposazenie, sep="", collapse =",")
    Ilosc_elementow <- stri_count(Car_wyposazenie, regex = ",") + 1
    
    # RAMKA DANYCH
    rd_cars <- tibble(Nazwa_Oferty = Car_nazwa, ID_Oferta = Car_ID_Oferty_Auta,
                      Marka = Car_marka, Model = Car_model, Cena_Brutto = Car_cena,
                      Rodzaj_Napedu = Car_naped, Typ_Skrzynia = Car_skrzynia_biegow,
                      Pojemnosc_Silnika = Car_pojemnosc_silnika, Typ_Paliwa = Car_paliwo,
                      Zuzycie_Paliwa_na_100km = Car_zuzycie_paliwa_na_100km_srednie,
                      Emisja_CO2 = Car_emisja_CO2, Klasa_Emisji = Car_klasa_emisji,
                      Przebieg_Km = Car_przebieg_km, Pierwsza_Rejestracja = Car_pierwsza_rejestracja,
                      Poprzedni_wlasciciel = Car_poprzedni_wlasciciel, Kraj = Car_Kraj,
                      Typ_Sprzedawcy = Car_Typ_Sprzedawcy, Moc_Silnika = Car_moc,
                      Liczba_Zdjec = Car_Liczba_zdjec, Wyposazenie = Car_wyposazenie, 
                      Liczba_elementow_wyposazenia = Ilosc_elementow)
                           
    
    calosc[[i]] <- rd_cars
}



calosc <- bind_rows(calosc)
write.csv(calosc, "dane_www.csv")

