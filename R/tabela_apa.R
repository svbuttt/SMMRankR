#' @title Generowanie Tabeli APA dla SMMRankR
#' @description Funkcja przeksztalca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowana tabele zgodna ze standardem APA (gotowa do prezentacji i publikacji).
#' @param x Obiekt wynikowy z funkcji pakietu (np. \code{topsis_smm_wynik}).
#' @param tytul Opcjonalny tytul tabeli.
#' @return Obiekt klasy \code{flextable} zgodny z APA.
#' @importFrom flextable autofit
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.topsis_smm_wynik <- function(x, tytul = "Wyniki metody TOPSIS") {
  df <- x$ranking
  
  names(df) <- c("Platforma SMM", "D+ (Odleglosc od Idealu)", "D- (Odleglosc od Antyidealu)", "Wspolczynnik (CC)", "Pozycja w Rankingu")
  
  df$`D+ (Odleglosc od Idealu)` <- round(df$`D+ (Odleglosc od Idealu)`, 3)
  df$`D- (Odleglosc od Antyidealu)` <- round(df$`D- (Odleglosc od Antyidealu)`, 3)
  df$`Wspolczynnik (CC)` <- round(df$`Wspolczynnik (CC)`, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 1", tytul),
    note = c("Uwaga. CC - Coefficient of Closeness (Wspolczynnik Bliskosci). Im wyzsza wartosc, tym lepsza platforma.")
  )
}

#' @export
tabela_apa.vikor_smm_wynik <- function(x, tytul = "Wyniki metody VIKOR") {
  df <- x$ranking
  
  names(df) <- c("Platforma SMM", "Wskaznik S (Uzytecznosc)", "Wskaznik R (Zal)", "Indeks Q (Kompromis)", "Pozycja w Rankingu")
  
  df$`Wskaznik S (Uzytecznosc)` <- round(df$`Wskaznik S (Uzytecznosc)`, 3)
  df$`Wskaznik R (Zal)` <- round(df$`Wskaznik R (Zal)`, 3)
  df$`Indeks Q (Kompromis)` <- round(df$`Indeks Q (Kompromis)`, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 2", tytul),
    note = c("Uwaga. S: uzytecznosc grupowa (maksymalizacja), R: indywidualny zal (minimalizacja), Q: ostateczny indeks kompromisu (im mniej, tym lepiej).")
  )
}

#' @export
tabela_apa.waspas_smm_wynik <- function(x, tytul = "Wyniki metody WASPAS") {
  df <- x$ranking
  
  names(df) <- c("Platforma SMM", "WSM (Model Sumaryczny)", "WPM (Model Iloczynowy)", "Wskaznik Q (WASPAS)", "Pozycja w Rankingu")
  
  df$`WSM (Model Sumaryczny)` <- round(df$`WSM (Model Sumaryczny)`, 3)
  df$`WPM (Model Iloczynowy)` <- round(df$`WPM (Model Iloczynowy)`, 3)
  df$`Wskaznik Q (WASPAS)` <- round(df$`Wskaznik Q (WASPAS)`, 4)
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = c("Uwaga. WASPAS laczy model sumowany (WSM) i iloczynowy (WPM) w jeden wspolny wskaznik uzytecznosci.")
  )
}

#' @export
tabela_apa.meta_smm_wynik <- function(x, tytul = "Wyniki Meta-Rankingu Platform SMM") {
  df <- x$porownanie
  
  names(df) <- gsub("_", " ", names(df))
  names(df)[names(df) == "Meta Konsensus RA"] <- "Algorytm Genetyczny (RA)"
  names(df)[names(df) == "Meta Dominacja"] <- "Regula Dominacji"
  names(df)[names(df) == "Meta Srednia Pozycja"] <- "Suma Pozycji (Borda)"
  names(df)[names(df) == "Miejsce TOPSIS"] <- "Miejsce TOPSIS"
  names(df)[names(df) == "Miejsce VIKOR"] <- "Miejsce VIKOR"
  names(df)[names(df) == "Miejsce WASPAS"] <- "Miejsce WASPAS"
  
  rempsyc::nice_table(
    df,
    title = c("Tabela 4", tytul),
    note = c("Uwaga. Zestawienie rang uzyskanych z trzech niezaleznych algorytmow (TOPSIS, VIKOR, WASPAS) oraz ostateczne wyznaczenie lidera za pomoca algorytmu konsensusu.")
  )
}
