#' Generowanie macierzy danych dla platform SMM
#'
#' @description Funkcja tworzy profesjonalną macierz decyzyjną z ocenami dla platform TikTok, Instagram i YouTube.
#' @return macierz_smm Macierz z danymi marketingowymi (Zasięg, Zaangażowanie, Koszt).
#' @export
stworz_macierz_smm <- function() {

  # Przygotowanie danych (Zasieg, Zaangazowanie, Koszt)
  # TikTok, Instagram, YouTube
  dane_wartosci <- matrix(c(
    90, 85, 30,
    75, 95, 60,
    65, 70, 45
  ), nrow = 3, byrow = TRUE)

  # Nazwy kryteriów i alternatyw zgodne z tematyką SMM
  colnames(dane_wartosci) <- c("Zasięg", "Zaangażowanie", "Koszt")
  rownames(dane_wartosci) <- c("TikTok", "Instagram", "YouTube")

  # Nadanie klasy S3, aby obiekt był rozpoznawalny w pakiecie [cite: 184]
  class(dane_wartosci) <- c("macierz_smm", "matrix")

  return(dane_wartosci)
}
