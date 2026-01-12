#' Wykres bubble dla wyników MCDA
#'
#' @param scores data.frame lub macierz z ocenami kryteriów
#' @param weights wektor wag kryteriów
#' @export
smm_bubble_plot <- function(scores, weights) {
  result <- smm_rank_score(scores, weights)
  x <- scores[, "Zasieg"]
  y <- scores[, "Zaangazowanie"]
  size <- result$WynikKoncowy * 5

  plot(x, y, cex = size, pch = 21, bg = "skyblue",
       xlab = "Zasieg", ylab = "Zaangazowanie",
       main = "Bubble plot wynikow MCDA")
  text(x, y, labels = rownames(scores), pos = 4, cex = 0.8, offset = 0.7)
}
