
#' Wykres bubble dla wynikow MCDA
#'
#' @param scores data.frame lub macierz z ocenami kryteriow
#' @param weights wektor wag kryteriow
#' @importFrom graphics text
#' @export
smm_bubble_plot <- function(scores, weights) {
  # Wywolanie funkcji rankingowej
  result <- smm_rank_score(scores, weights)

  # Pobranie danych do osi (upewnij sie, ze nazwy kolumn w dummy_data sa poprawne)
  x <- scores[, "Zasieg"]
  y <- scores[, "Zaangazowanie"]

  # Rozmiar kulek na podstawie obliczonego Score
  size <- result$Score * 5

  # Rysowanie wykresu
  plot(x, y, cex = size, pch = 21, bg = "skyblue",
       xlab = "Zasieg", ylab = "Zaangazowanie",
       main = "Bubble plot wynikow MCDA")

  # Uzycie graphics::text dla unikniecia bledow w NAMESPACE
  graphics::text(x, y, labels = rownames(scores), pos = 4, cex = 0.8, offset = 0.7)
}
