#' @title Generowanie Tabeli APA dla SMMRankR
#' @description Funkcja przeksztalca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowana tabele zgodna ze standardem APA (gotowa do prezentacji i publikacji).
#' @param x Obiekt wynikowy z funkcji pakietu (np. \code{topsis_smm_wynik}).
#' @param tytul Opcjonalny tytul tabeli.
#' @return Obiekt klasy \code{flextable} zgodny z APA.
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

#' @title Generowanie Tabeli HTML w Stylu APA dla SMMRankR
#' @description Funkcja przeksztalca wyniki analizy MCDA w sformatowana tabele HTML zgodna ze standardem APA.
#' @param x Obiekt wynikowy z funkcji pakietu (np. \code{topsis_smm_wynik}).
#' @param tytul Opcjonalny tytul tabeli.
#' @return Obiekt klasy \code{shiny.tag.list} zawierajacy tabele HTML.
#' @export
tabela_apa_html <- function(x, tytul = NULL) {
  UseMethod("tabela_apa_html")
}

#' @export
tabela_apa_html.topsis_smm_wynik <- function(x, tytul = "Wyniki metody TOPSIS") {
  df <- x$ranking
  names(df) <- c("Platforma SMM", "D+ (Odleglosc od Idealu)", "D- (Odleglosc od Antyidealu)", "Wspolczynnik (CC)", "Pozycja w Rankingu")
  df$`D+ (Odleglosc od Idealu)` <- round(df$`D+ (Odleglosc od Idealu)`, 3)
  df$`D- (Odleglosc od Antyidealu)` <- round(df$`D- (Odleglosc od Antyidealu)`, 3)
  df$`Wspolczynnik (CC)` <- round(df$`Wspolczynnik (CC)`, 4)
  
  .buduj_tabele_html_smm(
    df, 
    numer_tabeli = 1, 
    tytul = tytul, 
    notka = "Uwaga. CC - Coefficient of Closeness (Wspolczynnik Bliskosci). Im wyzsza wartosc, tym lepsza platforma."
  )
}

#' @export
tabela_apa_html.vikor_smm_wynik <- function(x, tytul = "Wyniki metody VIKOR") {
  df <- x$ranking
  names(df) <- c("Platforma SMM", "Wskaznik S (Uzytecznosc)", "Wskaznik R (Zal)", "Indeks Q (Kompromis)", "Pozycja w Rankingu")
  df$`Wskaznik S (Uzytecznosc)` <- round(df$`Wskaznik S (Uzytecznosc)`, 3)
  df$`Wskaznik R (Zal)` <- round(df$`Wskaznik R (Zal)`, 3)
  df$`Indeks Q (Kompromis)` <- round(df$`Indeks Q (Kompromis)`, 4)
  
  .buduj_tabele_html_smm(
    df, 
    numer_tabeli = 2, 
    tytul = tytul, 
    notka = "Uwaga. S: uzytecznosc grupowa (maksymalizacja), R: indywidualny zal (minimalizacja), Q: ostateczny indeks kompromisu (im mniej, tym lepiej)."
  )
}

#' @export
tabela_apa_html.waspas_smm_wynik <- function(x, tytul = "Wyniki metody WASPAS") {
  df <- x$ranking
  names(df) <- c("Platforma SMM", "WSM (Model Sumaryczny)", "WPM (Model Iloczynowy)", "Wskaznik Q (WASPAS)", "Pozycja w Rankingu")
  df$`WSM (Model Sumaryczny)` <- round(df$`WSM (Model Sumaryczny)`, 3)
  df$`WPM (Model Iloczynowy)` <- round(df$`WPM (Model Iloczynowy)`, 3)
  df$`Wskaznik Q (WASPAS)` <- round(df$`Wskaznik Q (WASPAS)`, 4)
  
  .buduj_tabele_html_smm(
    df, 
    numer_tabeli = 3, 
    tytul = tytul, 
    notka = "Uwaga. WASPAS laczy model sumowany (WSM) i iloczynowy (WPM) w jeden wskaznik uzytecznosci."
  )
}

#' @export
tabela_apa_html.meta_smm_wynik <- function(x, tytul = "Wyniki Meta-Rankingu Platform SMM") {
  df <- x$porownanie
  names(df) <- gsub("_", " ", names(df))
  names(df)[names(df) == "Meta Konsensus RA"] <- "Algorytm Genetyczny (RA)"
  names(df)[names(df) == "Meta Dominacja"] <- "Regula Dominacji"
  names(df)[names(df) == "Meta Srednia Pozycja"] <- "Suma Pozycji (Borda)"
  names(df)[names(df) == "Miejsce TOPSIS"] <- "Miejsce TOPSIS"
  names(df)[names(df) == "Miejsce VIKOR"] <- "Miejsce VIKOR"
  names(df)[names(df) == "Miejsce WASPAS"] <- "Miejsce WASPAS"
  
  .buduj_tabele_html_smm(
    df, 
    numer_tabeli = 4, 
    tytul = tytul, 
    notka = "Uwaga. Zestawienie rang uzyskanych z trzech niezaleznych algorytmow (TOPSIS, VIKOR, WASPAS) oraz ostateczne wyznaczenie lidera za pomoca algorytmu konsensusu."
  )
}

.buduj_tabele_html_smm <- function(df, numer_tabeli, tytul, notka) {
  cols <- names(df)
  
  header_row <- htmltools::tags$tr(
    lapply(seq_along(cols), function(i) {
      align_style <- if (i == 1) "text-align: left;" else "text-align: center;"
      htmltools::tags$th(
        style = paste("border-top: 2px solid #111111; border-bottom: 1px solid #111111; padding: 8px 10px; font-weight: bold;", align_style),
        cols[i]
      )
    })
  )
  
  body_rows <- lapply(1:nrow(df), function(r_idx) {
    row_data <- df[r_idx, ]
    is_last <- (r_idx == nrow(df))
    bottom_border <- if (is_last) "border-bottom: 2px solid #111111;" else ""
    
    htmltools::tags$tr(
      lapply(seq_along(cols), function(c_idx) {
        align_style <- if (c_idx == 1) "text-align: left;" else "text-align: center;"
        htmltools::tags$td(
          style = paste("padding: 6px 10px;", align_style, bottom_border),
          as.character(row_data[[c_idx]])
        )
      })
    )
  })
  
  htmltools::tagList(
    htmltools::tags$div(
      style = "font-family: 'Plus Jakarta Sans', sans-serif; color: #111111; width: 100%; max-width: 800px; margin: 10px auto; text-align: left;",
      htmltools::tags$div(style = "font-weight: bold; font-size: 13px; margin-bottom: 2px;", paste("Tabela", numer_tabeli)),
      htmltools::tags$div(style = "font-style: italic; font-size: 13px; margin-bottom: 8px;", tytul),
      htmltools::tags$table(
        style = "border-collapse: collapse; width: 100%; font-size: 12px; margin-bottom: 8px;",
        htmltools::tags$thead(header_row),
        htmltools::tags$tbody(body_rows)
      ),
      htmltools::tags$div(style = "font-size: 11px; font-style: italic; color: #444444; margin-top: 4px; line-height: 1.4;", notka)
    )
  )
}
