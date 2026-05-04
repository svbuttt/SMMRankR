#' Wyznaczanie wag kryteriów metodą Best-Worst (BWM)
#'
#' @description Funkcja oblicza wagi dla kryteriów SMM na podstawie wyboru najlepszego i najgorszego elementu.
#' @param najlepszy_indeks Indeks kryterium uznanego za najważniejsze
#' @param najgorszy_indeks Indeks kryterium uznanego za najmniej ważne
#' @param ile_kryteriow Całkowita liczba kryteriów w analizie
#' @return wagi_koncowe Wektor wag, których suma wynosi 1
#' @export
oblicz_wagi_smm_bwm <- function(najlepszy_indeks, najgorszy_indeks, ile_kryteriow) {

  # Inicjalizacja wektora (zgodnie z logiką zaprezentowaną przez Marka)
  wagi <- rep(1, ile_kryteriow)

  # Nadanie priorytetów: najlepszy kryterium otrzymuje najwyższą wartość
  wagi[najlepszy_indeks] <- 5
  wagi[najgorszy_indeks] <- 1

  # Pozostałe kryteria otrzymują wartość neutralną (średnią)
  pozostale_indeksy <- setdiff(1:ile_kryteriow, c(najlepszy_indeks, najgorszy_indeks))
  wagi[pozostale_indeksy] <- 3

  # Normalizacja wag, aby suma była równa 1
  wagi_koncowe <- wagi / sum(wagi)

  return(wagi_koncowe)
}
