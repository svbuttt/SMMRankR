#' @title Wewnetrzny parser modelu badawczego
#' @description Dekoduje definicje kryteriow MCDA podane w skladni zblizonej do lavaan (np. "Kryterium =~ zm1 + zm2").
#' @param skladnia Ciag znakow ze skladnia modelu.
#' @return Struktura listowa z mapowaniem kryteriow na zmienne surowe.
#' @keywords internal
.analizuj_skladnia_smm <- function(skladnia) {
  skladnia_czysta <- gsub("\n", "", skladnia)
  linie <- strsplit(skladnia_czysta, ";")[[1]]
  struktura_kryteriow <- list()
  
  for (linia in linie) {
    if (trimws(linia) == "") 
      next
    czesci <- strsplit(linia, "=~")[[1]]
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      struktura_kryteriow[[nazwa_kryterium]] <- elementy
    }
  }
  return(struktura_kryteriow)
}

#' @title Wewnetrzny skaler do przedzialu 1-9
#' @description Przeksztalca wektor wartosci liczbowych na jednolita skale od 1 do 9.
#' @param x Wektor numeryczny.
#' @return Wektor sformatowany do przedzialu [1, 9].
#' @keywords internal
.skaluj_do_1_9 <- function(x) {
  wazny <- !is.na(x)
  if (sum(wazny) == 0) return(x)
  
  min_val <- min(x[wazny])
  max_val <- max(x[wazny])
  
  if (min_val == max_val) {
    x[wazny] <- 5.0 # Wartosc srodkowa przy braku zroznicowania
  } else {
    x[wazny] <- 1.0 + (x[wazny] - min_val) * (8.0 / (max_val - min_val))
  }
  return(x)
}

#' Przygotowanie macierzy decyzyjnej SMM
#'
#' @description Funkcja przetwarza surowe dane marketingowe (np. ankiety, projekty) na sformatowana
#' macierz decyzyjna na podstawie podanej skladni kryteriow. Czyszczone sa kody bledow (999) i imputowane
#' braki danych (NA) za pomoca sredniej grupowej dla danej platformy.
#'
#' @param dane Ramka danych z surowymi zmiennymi.
#' @param skladnia Formula grupujaca zmienne surowe w kryteria kompozytowe (skladnia lavaan).
#' @param kolumna_platformy Nazwa kolumny zawierajacej identyfikatory platform (alternatyw). Domyslnie "Platforma".
#' @param agregacja Funkcja agregujaca wiersze do poziomu platform. Domyslnie `mean`.
#' @return Macierz decyzyjna (platformy x kryteria) z odpowiednimi nazwami wierszy i kolumn.
#' @export
zbuduj_macierz_decyzyjna <- function(dane, skladnia, kolumna_platformy = "Platforma", agregacja = mean) {
  if (!is.data.frame(dane)) {
    stop("Argument 'dane' musi byc ramka danych (data frame).")
  }
  if (!kolumna_platformy %in% names(dane)) {
    stop(sprintf("Kolumna '%s' nie istnieje w podanych danych.", kolumna_platformy))
  }
  
  # 1. Czyszczenie kodow bledow 999 i ujemnych anomalii
  dane_czyste <- dane
  for (col in names(dane_czyste)) {
    if (is.numeric(dane_czyste[[col]])) {
      # Zamiana kodu bledu 999 oraz ujemnych wartosci na NA
      dane_czyste[[col]][dane_czyste[[col]] == 999] <- NA
      dane_czyste[[col]][dane_czyste[[col]] < 0] <- NA
      
      # Imputacja brakow danych (NA) srednia dla danej platformy
      if (any(is.na(dane_czyste[[col]]))) {
        for (plat in unique(dane_czyste[[kolumna_platformy]])) {
          indeksy_plat <- dane_czyste[[kolumna_platformy]] == plat
          wektor_plat <- dane_czyste[[col]][indeksy_plat]
          
          if (all(is.na(wektor_plat))) {
            # Jesli brak jakichkolwiek nie-NA, podstawiamy srednia globalna
            srednia_val <- mean(dane_czyste[[col]], na.rm = TRUE)
            if (is.nan(srednia_val)) srednia_val <- 0
            dane_czyste[[col]][indeksy_plat & is.na(dane_czyste[[col]])] <- srednia_val
          } else {
            srednia_plat <- mean(wektor_plat, na.rm = TRUE)
            dane_czyste[[col]][indeksy_plat & is.na(dane_czyste[[col]])] <- srednia_plat
          }
        }
      }
    }
  }
  
  # 2. Parsowanie skladni kryteriow
  struktura <- .analizuj_skladnia_smm(skladnia)
  nazwy_kryteriow <- names(struktura)
  if (length(nazwy_kryteriow) == 0) {
    stop("Blad skladni: Nie zdefiniowano zadnych kryteriow.")
  }
  
  # 3. Obliczanie zmiennych kompozytowych i skalowanie do przedzialu 1-9
  wyniki_kompozytowe <- data.frame(Platforma_ID = dane_czyste[[kolumna_platformy]])
  
  for (kryt in nazwy_kryteriow) {
    zmienne <- struktura[[kryt]]
    brakujace <- zmienne[!zmienne %in% names(dane_czyste)]
    if (length(brakujace) > 0) {
      stop(sprintf("Blad skladni: Zmienne %s nie istnieja w danych.", paste(brakujace, collapse = ", ")))
    }
    
    # Wyznaczenie sredniej z wierszy dla zmiennych wchodzacych w sklad kryterium
    if (length(zmienne) > 1) {
      surowy_kompozyt <- rowMeans(dane_czyste[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_kompozyt <- dane_czyste[[zmienne]]
    }
    
    # Skalowanie kompozytu do skali 1-9
    wyniki_kompozytowe[[kryt]] <- .skaluj_do_1_9(surowy_kompozyt)
  }
  
  # 4. Agregacja wierszy do poziomu platformy (np. srednia z 20 kampanii)
  zagregowane <- stats::aggregate(
    . ~ Platforma_ID,
    data = wyniki_kompozytowe,
    FUN = agregacja
  )
  
  # Sortowanie platform alfabetycznie dla stabilnosci
  zagregowane <- zagregowane[order(zagregowane$Platforma_ID), ]
  
  # Przygotowanie wyjsciowej macierzy decyzyjnej
  macierz_wyjsciowa <- as.matrix(zagregowane[, nazwy_kryteriow, drop = FALSE])
  rownames(macierz_wyjsciowa) <- zagregowane$Platforma_ID
  
  class(macierz_wyjsciowa) <- c("macierz_smm", "matrix")
  attr(macierz_wyjsciowa, "nazwy_kryteriow") <- nazwy_kryteriow
  
  return(macierz_wyjsciowa)
}
