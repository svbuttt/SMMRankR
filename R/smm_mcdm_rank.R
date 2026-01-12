#' Rankowanie platform i typów treści w mediach społecznościowych
#'
#' @param scores data.frame lub macierz z ocenami kryteriów
#' @param weights wektor wag kryteriów
#' @return data.frame z wynikami rankingu
#' @export
smm_rank_score <- function(scores, weights) {

  if (ncol(scores) != length(weights)) {
    stop("Liczba kryteriów musi odpowiadać długości wag")
  }

  scores <- as.matrix(scores)
  normalized <- sweep(scores, 2, colSums(scores), "/")
  final_score <- normalized %*% weights

  result <- data.frame(
    Alternatywa = rownames(scores),
    WynikKoncowy = round(as.numeric(final_score), 3)
  )

  result[order(-result$WynikKoncowy), ]
}
