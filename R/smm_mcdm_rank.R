#' Ranking platform spolecznosciowych metoda MCDA
#'
#' @param scores Macierz lub data.frame z ocenami alternatyw (wiersze to platformy, kolumny to kryteria)
#' @param weights Wektor wag dla kryteriow (suma wag powinna wynosic 1)
#' @return Data.frame z wynikami rankingu i punktacja
#' @export
smm_rank_score <- function(scores, weights) {

  # 1. Walidacja typu danych
  if (!is.matrix(scores) && !is.data.frame(scores)) {
    stop("Blad: 'scores' musi byc macierza lub data.frame.")
  }

  if (!is.numeric(weights)) {
    stop("Blad: 'weights' musi byc wektorem liczbowym.")
  }

  # 2. Walidacja wymiarow
  if (ncol(scores) != length(weights)) {
    stop("Blad: Liczba kolumn w 'scores' musi odpowiadac dlugosci wektora wag.")
  }

  # 3. Obsluga brakujacych danych (NA)
  if (any(is.na(scores))) {
    warning("Dane zawieraja wartosci NA. Zostana one zastapione przez 0.")
    scores[is.na(scores)] <- 0
  }

  # 4. Normalizacja (SAW - Simple Additive Weighting)
  # Dzielimy kazda wartosc przez sume kolumny
  col_sums <- colSums(scores)
  if (any(col_sums == 0)) {
    stop("Blad: Jedna z kolumn zawiera same zera. Normalizacja niemozliwa.")
  }

  norm_scores <- sweep(scores, 2, col_sums, "/")

  # 5. Obliczenie wyniku koncowego (Weighted Sum)
  final_scores <- as.numeric(norm_scores %*% weights)

  # 6. Przygotowanie wynikow
  results <- data.frame(
    Alternative = rownames(scores),
    Score = round(final_scores, 4)
  )

  # Jesli brak nazw wierszy, nadaj im numery
  if (is.null(results$Alternative) || any(results$Alternative == "")) {
    results$Alternative <- paste("Alt", 1:nrow(results))
  }

  # Dodanie rankingu
  results$Rank <- rank(-results$Score, ties.method = "first")

  # Sortowanie
  results <- results[order(results$Rank), ]

  return(results)
}
