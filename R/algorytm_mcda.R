#' @title Ranking SMM metodą TOPSIS
#' @description Funkcja wykonuje pełną procedurę rankingu TOPSIS dla danych ostrych (crisp).
#' @param tablica_wynikow Macierz danych (alternatywy x kryteria) np. z funkcji zbuduj_macierz_decyzyjna.
#' @param wagi_smm Wektor wag kryteriów sumujący się do 1.
#' @param kierunki Wektor znakowy określający kierunki kryteriów ("max" dla zysku, "min" dla kosztu). Domyślnie "max".
#' @return Obiekt klasy \code{topsis_smm_wynik} zawierający tabelę rankingu oraz parametry.
#' @export
oblicz_topsis_smm <- function(tablica_wynikow, wagi_smm, kierunki = NULL) {
  if (!is.matrix(tablica_wynikow)) {
    tablica_wynikow <- as.matrix(tablica_wynikow)
  }
  
  ile_kryteriow <- ncol(tablica_wynikow)
  ile_alternatyw <- nrow(tablica_wynikow)
  
  if (is.null(kierunki)) {
    kierunki <- rep("max", ile_kryteriow)
  }
  
  # 1. Normalizacja wektorowa
  kwadraty_sum <- colSums(tablica_wynikow^2)
  kwadraty_sum[kwadraty_sum == 0] <- 1e-9
  macierz_znormalizowana <- sweep(tablica_wynikow, 2, sqrt(kwadraty_sum), "/")
  
  # 2. Tworzenie macierzy zważonej
  macierz_zwazona <- sweep(macierz_znormalizowana, 2, wagi_smm, "*")
  
  # 3. Wyznaczenie punktów idealnych (PIS) i anty-idealnych (NIS)
  pis <- numeric(ile_kryteriow)
  nis <- numeric(ile_kryteriow)
  
  for (j in 1:ile_kryteriow) {
    if (kierunki[j] == "max") {
      pis[j] <- max(macierz_zwazona[, j])
      nis[j] <- min(macierz_zwazona[, j])
    } else {
      pis[j] <- min(macierz_zwazona[, j])
      nis[j] <- max(macierz_zwazona[, j])
    }
  }
  
  # 4. Obliczanie odległości euklidesowych
  odleglosc_plus <- sqrt(rowSums(sweep(macierz_zwazona, 2, pis, "-")^2))
  odleglosc_minus <- sqrt(rowSums(sweep(macierz_zwazona, 2, nis, "-")^2))
  
  # 5. Obliczanie współczynnika CC (Closeness Coefficient)
  mianownik <- odleglosc_plus + odleglosc_minus
  mianownik[mianownik == 0] <- 1e-9
  wspolczynnik_score <- odleglosc_minus / mianownik
  
  # Przygotowanie wyniku końcowego
  ranking_koncowy <- data.frame(
    Platforma = rownames(tablica_wynikow),
    Dystans_Od_Idealu = round(odleglosc_plus, 4),
    Dystans_Od_Antyidealu = round(odleglosc_minus, 4),
    Wskaznik_CC = round(wspolczynnik_score, 4),
    Pozycja_w_Rankingu = rank(-wspolczynnik_score, ties.method = "first"),
    stringsAsFactors = FALSE
  )
  
  # Sortowanie od najlepszego wyniku
  ranking_koncowy <- ranking_koncowy[order(ranking_koncowy$Pozycja_w_Rankingu), ]
  rownames(ranking_koncowy) <- NULL
  
  wynik <- list(
    ranking = ranking_koncowy,
    wykorzystana_metoda = "TOPSIS (Crisp)",
    dane_wejsciowe = tablica_wynikow,
    wagi = wagi_smm,
    kierunki = kierunki
  )
  
  class(wynik) <- c("topsis_smm_wynik", "list")
  return(wynik)
}

#' @title Ranking SMM metodą VIKOR
#' @description Wykonuje procedurę kompromisowego rankingu VIKOR dla danych ostrych.
#' @param tablica_wynikow Macierz decyzyjna (alternatywy x kryteria).
#' @param wagi_smm Wektor wag kryteriów sumujący się do 1.
#' @param kierunki Wektor znakowy określający kierunki kryteriów ("max" lub "min").
#' @param v Parametr wagowy strategii "większości kryteriów" (zwykle 0.5).
#' @return Obiekt klasy \code{vikor_smm_wynik} z tabelą rankingu i detalami.
#' @export
oblicz_vikor_smm <- function(tablica_wynikow, wagi_smm, kierunki = NULL, v = 0.5) {
  if (!is.matrix(tablica_wynikow)) {
    tablica_wynikow <- as.matrix(tablica_wynikow)
  }
  
  m <- nrow(tablica_wynikow)
  n <- ncol(tablica_wynikow)
  
  if (is.null(kierunki)) {
    kierunki <- rep("max", n)
  }
  
  # 1. Wyznaczenie wartości najlepszych i najgorszych
  best <- numeric(n)
  worst <- numeric(n)
  
  for (j in 1:n) {
    if (kierunki[j] == "max") {
      best[j] <- max(tablica_wynikow[, j])
      worst[j] <- min(tablica_wynikow[, j])
    } else {
      best[j] <- min(tablica_wynikow[, j])
      worst[j] <- max(tablica_wynikow[, j])
    }
  }
  
  # 2. Obliczenie znormalizowanych odległości i ważenie
  d_mat <- matrix(0, nrow = m, ncol = n)
  for (j in 1:n) {
    mianownik <- best[j] - worst[j]
    if (mianownik == 0) mianownik <- 1e-9
    d_mat[, j] <- wagi_smm[j] * (best[j] - tablica_wynikow[, j]) / mianownik
  }
  
  # 3. Wyznaczenie wskaźników S oraz R
  S <- rowSums(d_mat)
  R <- apply(d_mat, 1, max)
  
  # 4. Wyznaczenie wskaźnika kompromisu Q
  S_star <- min(S)
  S_minus <- max(S)
  R_star <- min(R)
  R_minus <- max(R)
  
  mianownik_S <- S_minus - S_star
  if (mianownik_S == 0) mianownik_S <- 1
  
  mianownik_R <- R_minus - R_star
  if (mianownik_R == 0) mianownik_R <- 1
  
  Q <- v * (S - S_star) / mianownik_S + (1 - v) * (R - R_star) / mianownik_R
  
  # Przygotowanie rankingu (im mniejsze Q tym lepiej)
  ranking_koncowy <- data.frame(
    Platforma = rownames(tablica_wynikow),
    Wskaznik_S = round(S, 4),
    Wskaznik_R = round(R, 4),
    Indeks_Q = round(Q, 4),
    Pozycja_w_Rankingu = rank(Q, ties.method = "first"),
    stringsAsFactors = FALSE
  )
  
  ranking_koncowy <- ranking_koncowy[order(ranking_koncowy$Pozycja_w_Rankingu), ]
  rownames(ranking_koncowy) <- NULL
  
  wynik <- list(
    ranking = ranking_koncowy,
    wykorzystana_metoda = "VIKOR (Crisp)",
    dane_wejsciowe = tablica_wynikow,
    wagi = wagi_smm,
    kierunki = kierunki,
    v = v
  )
  
  class(wynik) <- c("vikor_smm_wynik", "list")
  return(wynik)
}

#' @title Ranking SMM metodą WASPAS
#' @description Wykonuje procedurę rankingu WASPAS (Weighted Aggregated Sum Product Assessment).
#' @param tablica_wynikow Macierz decyzyjna (alternatywy x kryteria).
#' @param wagi_smm Wektor wag kryteriów sumujący się do 1.
#' @param kierunki Wektor znakowy określający kierunki kryteriów ("max" lub "min").
#' @param lambda Parametr równowagi między modelem sumarycznym WSM a iloczynowym WPM. Domyślnie 0.5.
#' @return Obiekt klasy \code{waspas_smm_wynik} z tabelą rankingu.
#' @export
oblicz_waspas_smm <- function(tablica_wynikow, wagi_smm, kierunki = NULL, lambda = 0.5) {
  if (!is.matrix(tablica_wynikow)) {
    tablica_wynikow <- as.matrix(tablica_wynikow)
  }
  
  m <- nrow(tablica_wynikow)
  n <- ncol(tablica_wynikow)
  
  if (is.null(kierunki)) {
    kierunki <- rep("max", n)
  }
  
  # 1. Normalizacja liniowa
  n_mat <- matrix(0, nrow = m, ncol = n)
  for (j in 1:n) {
    col_max <- max(tablica_wynikow[, j])
    col_min <- min(tablica_wynikow[, j])
    
    if (kierunki[j] == "max") {
      if (col_max == 0) col_max <- 1e-9
      n_mat[, j] <- tablica_wynikow[, j] / col_max
    } else {
      # Pomijamy dzielenie przez zero dla minimów
      zera <- tablica_wynikow[, j] == 0
      tablica_wynikow_bez_zer <- tablica_wynikow[, j]
      tablica_wynikow_bez_zer[zera] <- 1e-9
      n_mat[, j] <- col_min / tablica_wynikow_bez_zer
    }
  }
  
  # 2. Model Sumy Ważonej (WSM)
  WSM <- rowSums(sweep(n_mat, 2, wagi_smm, "*"))
  
  # 3. Model Iloczynu Ważonego (WPM)
  WPM <- apply(sweep(n_mat, 2, wagi_smm, "^"), 1, prod)
  
  # 4. Joint score WASPAS Q
  Q <- lambda * WSM + (1 - lambda) * WPM
  
  ranking_koncowy <- data.frame(
    Platforma = rownames(tablica_wynikow),
    Wynik_WSM = round(WSM, 4),
    Wynik_WPM = round(WPM, 4),
    Wskaznik_Q_WASPAS = round(Q, 4),
    Pozycja_w_Rankingu = rank(-Q, ties.method = "first"),
    stringsAsFactors = FALSE
  )
  
  ranking_koncowy <- ranking_koncowy[order(ranking_koncowy$Pozycja_w_Rankingu), ]
  rownames(ranking_koncowy) <- NULL
  
  wynik <- list(
    ranking = ranking_koncowy,
    wykorzystana_metoda = "WASPAS (Crisp)",
    dane_wejsciowe = tablica_wynikow,
    wagi = wagi_smm,
    kierunki = kierunki,
    lambda = lambda
  )
  
  class(wynik) <- c("waspas_smm_wynik", "list")
  return(wynik)
}

# Wewnętrzna pomocnicza funkcja dominacji
.oblicz_dominacje_smm <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)
  
  for (obecna_pozycja in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf
    
    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])
    najlepszy_r3 <- which.min(obecna_macierz[, 3])
    
    kandydaci <- c(najlepszy_r1, najlepszy_r2, najlepszy_r3)
    tabela_czestosci <- table(kandydaci)
    zwyciezca_idx <- as.numeric(names(tabela_czestosci)[which.max(tabela_czestosci)])
    
    if (length(tabela_czestosci) == 3) {
      c1 <- najlepszy_r1; c2 <- najlepszy_r2; c3 <- najlepszy_r3
      
      c1_wygrane <- sum(macierz_rang[c1, ] < macierz_rang[c2, ]) + sum(macierz_rang[c1, ] < macierz_rang[c3, ])
      c2_wygrane <- sum(macierz_rang[c2, ] < macierz_rang[c1, ]) + sum(macierz_rang[c2, ] < macierz_rang[c3, ])
      c3_wygrane <- sum(macierz_rang[c3, ] < macierz_rang[c1, ]) + sum(macierz_rang[c3, ] < macierz_rang[c2, ])
      
      wygrane <- c(c1_wygrane, c2_wygrane, c3_wygrane)
      
      if (which.max(wygrane) == 1) zwyciezca_idx <- c1
      else if (which.max(wygrane) == 2) zwyciezca_idx <- c2
      else zwyciezca_idx <- c3
    }
    
    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] = FALSE
  }
  return(finalny_ranking)
}

#' @title Meta-Ranking platform SMM
#' @description Agreguje wyniki z metod TOPSIS, VIKOR i WASPAS w jeden spójny meta-ranking konsensusu.
#' @param tablica_wynikow Macierz decyzyjna (alternatywy x kryteria).
#' @param wagi_smm Wektor wag kryteriów sumujący się do 1.
#' @param kierunki Wektor znakowy kierunków kryteriów ("max" lub "min").
#' @param v Parametr dla VIKOR (domyślnie 0.5).
#' @param lambda Parametr dla WASPAS (domyślnie 0.5).
#' @return Obiekt klasy \code{meta_smm_wynik} zawierający tabelę porównawczą i macierz korelacji.
#' @importFrom RankAggreg BruteAggreg
#' @export
oblicz_meta_ranking_smm <- function(tablica_wynikow, wagi_smm, kierunki = NULL, v = 0.5, lambda = 0.5) {
  if (!is.matrix(tablica_wynikow)) {
    tablica_wynikow <- as.matrix(tablica_wynikow)
  }
  
  ile_alts <- nrow(tablica_wynikow)
  
  # Uruchomienie trzech metod
  topsis_res <- oblicz_topsis_smm(tablica_wynikow, wagi_smm, kierunki)
  vikor_res <- oblicz_vikor_smm(tablica_wynikow, wagi_smm, kierunki, v)
  waspas_res <- oblicz_waspas_smm(tablica_wynikow, wagi_smm, kierunki, lambda)
  
  # Ekstrakcja rang alfabetycznie według nazw platform
  topsis_sorted <- topsis_res$ranking[order(topsis_res$ranking$Platforma), ]
  vikor_sorted <- vikor_res$ranking[order(vikor_res$ranking$Platforma), ]
  waspas_sorted <- waspas_res$ranking[order(waspas_res$ranking$Platforma), ]
  
  r_topsis <- topsis_sorted$Pozycja_w_Rankingu
  r_vikor <- vikor_sorted$Pozycja_w_Rankingu
  r_waspas <- waspas_sorted$Pozycja_w_Rankingu
  nazwy_platform <- topsis_sorted$Platforma
  
  # 1. Agregacja Borda (Suma Pozycji)
  suma_pozycji <- r_topsis + r_vikor + r_waspas
  ranking_borda <- rank(suma_pozycji, ties.method = "first")
  
  # 2. Agregacja Dominacją (Głosowanie większościowe)
  ranking_dominacja <- .oblicz_dominacje_smm(r_topsis, r_vikor, r_waspas)
  
  # 3. Konsensus RankAggreg (Algorytm Genetyczny lub Brute Force)
  macierz_dla_ra <- rbind(order(r_topsis), order(r_vikor), order(r_waspas))
  
  # BruteAggreg dla małej liczby alternatyw (<= 10) jest bardzo szybki i dokładny
  ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, ile_alts, distance = "Spearman")
  top_lista <- ra_wynik$top.list
  
  ranking_consensus <- numeric(ile_alts)
  for (pozycja in 1:ile_alts) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    ranking_consensus[indeks_alternatywy] <- pozycja
  }
  
  # Zestawienie porównawcze
  porownanie_df <- data.frame(
    Platforma = nazwy_platform,
    Miejsce_TOPSIS = r_topsis,
    Miejsce_VIKOR = r_vikor,
    Miejsce_WASPAS = r_waspas,
    Meta_Srednia_Pozycja = ranking_borda,
    Meta_Dominacja = ranking_dominacja,
    Meta_Konsensus_RA = ranking_consensus,
    stringsAsFactors = FALSE
  )
  
  # Sortowanie po ostatecznym konsensusie
  porownanie_df <- porownanie_df[order(porownanie_df$Meta_Konsensus_RA), ]
  rownames(porownanie_df) <- NULL
  
  # Macierz korelacji rang Spearmana
  macierz_kor <- stats::cor(porownanie_df[, c("Miejsce_TOPSIS", "Miejsce_VIKOR", "Miejsce_WASPAS")], method = "spearman")
  
  wynik <- list(
    porownanie = porownanie_df,
    zgodnosc_metod_korelacja = macierz_kor,
    wykorzystana_metoda = "Meta-Ranking SMM",
    dane_wejsciowe = tablica_wynikow
  )
  
  class(wynik) <- c("meta_smm_wynik", "list")
  return(wynik)
}
