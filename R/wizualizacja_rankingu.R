#' Wykres bąbelkowy wyników rankingu SMM
#'
#' @description Tworzy mapę strategiczną platform społecznościowych na podstawie wyników MCDA.
#' @param macierz_danych Macierz z danymi marketingowymi.
#' @param wyniki_rankingu Wyniki obliczone funkcją oblicz_topsis_smm.
#' @export
rysuj_ranking_smm <- function(macierz_danych, wyniki_rankingu) {
  x_os <- macierz_danych[, "Zasięg"]
  y_os <- macierz_danych[, "Zaangażowanie"]
  rozmiar <- wyniki_rankingu$Ocena * 15

  plot(x_os, y_os,
       cex = rozmiar,
       pch = 21,
       bg = "skyblue",
       col = "darkblue",
       xlab = "Zasięg",
       ylab = "Zaangażowanie",
       main = "Mapa Strategiczna Platform SMM")


  graphics::text(x_os, y_os, labels = rownames(macierz_danych), pos = 3, cex = 0.8)
  graphics::grid(col = "lightgray", lty = "dotted")
}
