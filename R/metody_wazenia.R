#' @title Wagi Entropii Shannona
#' @description Oblicza obiektywne wagi kryteriów na podstawie miary entropii (rozproszenia informacji).
#' @param macierz Macierz decyzyjna (alternatywy x kryteria).
#' @return Wektor wag kryteriów sumujący się do 1.
#' @export
oblicz_wagi_entropia <- function(macierz) {
  if (!is.matrix(macierz)) {
    macierz <- as.matrix(macierz)
  }
  
  m <- nrow(macierz)
  n <- ncol(macierz)
  
  # Normalizacja sumacyjna (p_ij = x_ij / sum(x_ij))
  kolumny_sumy <- colSums(macierz)
  kolumny_sumy[kolumny_sumy == 0] <- 1e-9 # Unikanie dzielenia przez zero
  
  p_mat <- sweep(macierz, 2, kolumny_sumy, "/")
  
  # Obliczanie wskaźnika Entropii E_j
  k_const <- 1 / log(m)
  entropies <- numeric(n)
  
  for (j in 1:n) {
    p_col <- p_mat[, j]
    p_col <- p_col[p_col > 0] # Pomijamy zera, log(0) nie istnieje
    if (length(p_col) == 0) {
      entropies[j] <- 1.0
    } else {
      entropies[j] <- -k_const * sum(p_col * log(p_col))
    }
  }
  
  # Stopień rozbieżności d_j i wagi ostateczne
  d <- 1.0 - entropies
  d_sum <- sum(d)
  
  if (d_sum == 0) {
    return(rep(1 / n, n))
  }
  
  wagi <- d / d_sum
  names(wagi) <- colnames(macierz)
  return(wagi)
}

#' @title Wagi CRITIC
#' @description Oblicza wagi kryteriów metodą CRITIC (Criteria Importance Through Intercriteria Correlation).
#' Uwzględnia kontrast intensywności (odchylenie standardowe) oraz konflikt między kryteriami (korelacje).
#' @param macierz Macierz decyzyjna (alternatywy x kryteria).
#' @param kierunki Opcjonalny wektor kierunków kryteriów ("max" lub "min"). Domyślnie wszystkie są traktowane jako "max".
#' @return Wektor wag kryteriów sumujący się do 1.
#' @export
oblicz_wagi_critic <- function(macierz, kierunki = NULL) {
  if (!is.matrix(macierz)) {
    macierz <- as.matrix(macierz)
  }
  
  m <- nrow(macierz)
  n <- ncol(macierz)
  
  if (is.null(kierunki)) {
    kierunki <- rep("max", n)
  }
  
  # Normalizacja MinMax w zależności od kierunku
  n_mat <- matrix(0, nrow = m, ncol = n)
  colnames(n_mat) <- colnames(macierz)
  
  for (j in 1:n) {
    col_min <- min(macierz[, j])
    col_max <- max(macierz[, j])
    mianownik <- col_max - col_min
    if (mianownik == 0) mianownik <- 1e-9
    
    if (kierunki[j] == "max") {
      n_mat[, j] <- (macierz[, j] - col_min) / mianownik
    } else {
      n_mat[, j] <- (col_max - macierz[, j]) / mianownik
    }
  }
  
  # Odchylenie standardowe dla każdego znormalizowanego kryterium
  std_devs <- apply(n_mat, 2, stats::sd)
  
  # Macierz korelacji Pearsona
  cor_mat <- stats::cor(n_mat, method = "pearson")
  # Obsługa ewentualnych wartości NA w korelacji (np. przy braku zmienności)
  cor_mat[is.na(cor_mat)] <- 0
  
  # Obliczanie C_j = std_j * sum(1 - corr_jk)
  C <- std_devs * colSums(1 - cor_mat)
  C_sum <- sum(C)
  
  if (C_sum == 0) {
    return(rep(1 / n, n))
  }
  
  wagi <- C / C_sum
  names(wagi) <- colnames(macierz)
  return(wagi)
}

#' @title Wagi Odchylenia Standardowego
#' @description Wyznacza obiektywne wagi kryteriów na podstawie odchylenia standardowego po normalizacji Min-Max.
#' @param macierz Macierz decyzyjna (alternatywy x kryteria).
#' @param kierunki Opcjonalny wektor kierunków kryteriów ("max" lub "min"). Domyślnie wszystkie są traktowane jako "max".
#' @return Wektor wag kryteriów sumujący się do 1.
#' @export
oblicz_wagi_std_dev <- function(macierz, kierunki = NULL) {
  if (!is.matrix(macierz)) {
    macierz <- as.matrix(macierz)
  }
  
  m <- nrow(macierz)
  n <- ncol(macierz)
  
  if (is.null(kierunki)) {
    kierunki <- rep("max", n)
  }
  
  # Normalizacja Min-Max
  n_mat <- matrix(0, nrow = m, ncol = n)
  colnames(n_mat) <- colnames(macierz)
  
  for (j in 1:n) {
    col_min <- min(macierz[, j])
    col_max <- max(macierz[, j])
    mianownik <- col_max - col_min
    if (mianownik == 0) mianownik <- 1e-9
    
    if (kierunki[j] == "max") {
      n_mat[, j] <- (macierz[, j] - col_min) / mianownik
    } else {
      n_mat[, j] <- (col_max - macierz[, j]) / mianownik
    }
  }
  
  # Obliczenie odchyleń standardowych
  std_devs <- apply(n_mat, 2, stats::sd)
  std_sum <- sum(std_devs)
  
  if (std_sum == 0) {
    return(rep(1 / n, n))
  }
  
  wagi <- std_devs / std_sum
  names(wagi) <- colnames(macierz)
  return(wagi)
}
