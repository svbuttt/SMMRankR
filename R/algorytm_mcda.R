#' Ranking platform SMM metodą TOPSIS
#'
#' @description Funkcja wykonuje pełną procedurę rankingu TOPSIS dla wybranych platform społecznościowych.
#' @param tablica_wynikow Macierz danych (np. TikTok, Instagram) wygenerowana funkcją stworz_macierz_smm.
#' @param wagi_smm Wektor wag kryteriów obliczony metodą BWM.
#' @return ranking_koncowy Ramka danych z pozycjami w rankingu.
#' @export
oblicz_topsis_smm <- function(tablica_wynikow, wagi_smm) {

  # 1. Normalizacja wektorowa (liczba_kryteriow - unikalna nazwa wg instrukcji)
  ile_kryteriow <- ncol(tablica_wynikow)
  macierz_znormalizowana <- sweep(tablica_wynikow, 2, sqrt(colSums(tablica_wynikow^2)), "/")

  # 2. Tworzenie macierzy zważonej
  macierz_zwazona <- sweep(macierz_znormalizowana, 2, wagi_smm, "*")

  # 3. Wyznaczenie punktów idealnych (PIS) i anty-idealnych (NIS)
  rozwiazanie_idealne <- apply(macierz_zwazona, 2, max)
  rozwiazanie_antyidealne <- apply(macierz_zwazona, 2, min)

  # 4. Obliczanie odległości euklidesowych
  odleglosc_plus <- sqrt(rowSums(sweep(macierz_zwazona, 2, rozwiazanie_idealne, "-")^2))
  odleglosc_minus <- sqrt(rowSums(sweep(macierz_zwazona, 2, rozwiazanie_antyidealne, "-")^2))

  # 5. Obliczanie współczynnika rankingu
  wspolczynnik_score <- odleglosc_minus / (odleglosc_plus + odleglosc_minus)

  # Przygotowanie wyniku końcowego
  ranking_koncowy <- data.frame(
    Platforma = rownames(tablica_wynikow),
    Ocena = round(wspolczynnik_score, 4)
  )

  # Sortowanie od najlepszego wyniku
  ranking_koncowy <- ranking_koncowy[order(-ranking_koncowy$Ocena), ]

  # Nadanie klasy S3 dla obiektów (wymóg Marka dla "normalnego" pakietu)
  class(ranking_koncowy) <- c("smm_ranking", "data.frame")

  return(ranking_koncowy)
}
