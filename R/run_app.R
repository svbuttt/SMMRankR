#' Uruchomienie aplikacji Shiny dla SMMRankR
#'
#' @description Uruchamia interaktywna aplikacje Shiny sluzaca do analizy wielokryterialnej platform spolecznosciowych.
#' @param ... Dodatkowe parametry przekazywane do funkcji \code{shiny::runApp}.
#' @export
uruchom_aplikacje <- function(...) {
  app_dir <- system.file("shiny-app", package = "SMMRankR")
  if (app_dir == "") {
    stop("Nie znaleziono katalogu aplikacji Shiny w pakiecie. Sprobuj przeinstalowac pakiet SMMRankR.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
