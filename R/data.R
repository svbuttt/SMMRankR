#' Przykładowe surowe dane marketingowe dla platform SMM
#'
#' Syntetyczny zbiór danych zawierający wyniki 120 kampanii marketingowych
#' przeprowadzonych na 6 platformach społecznościowych. Zbiór zawiera celowo
#' wprowadzone błędy (np. 999) oraz braki danych (NA) do celów demonstracyjnych.
#'
#' @format Ramka danych o 120 wierszach i 14 kolumnach:
#' \describe{
#'   \item{Kampania_ID}{Unikalny identyfikator kampanii (1-120)}
#'   \item{Platforma}{Platforma społecznościowa (TikTok, Instagram, YouTube, Facebook, LinkedIn, Pinterest)}
#'   \item{zasieg_surowy}{Surowy zasięg kampanii (wyświetlenia)}
#'   \item{liczba_klikniec}{Liczba kliknięć (z błędami 999)}
#'   \item{wspolczynnik_konwersji}{Współczynnik konwersji w procentach (z brakami NA)}
#'   \item{polubienia}{Liczba polubień}
#'   \item{komentarze}{Liczba komentarzy}
#'   \item{udostepnienia}{Liczba udostępnień}
#'   \item{koszt_kampanii}{Koszt kampanii w PLN}
#'   \item{cpc}{Koszt za kliknięcie (Cost Per Click) w PLN}
#'   \item{cpm}{Koszt za tysiąc wyświetleń (Cost Per Mille) w PLN}
#'   \item{satysfakcja_klienta}{Zadowolenie klienta z kampanii (Likert 1-5, z błędami 999)}
#'   \item{latwosc_obslugi}{Łatwość konfiguracji/obsługi platformy (Likert 1-5)}
#'   \item{jakosc_wspolpracy}{Jakość współpracy z influencerami/wsparciem (Likert 1-5, z brakami NA)}
#' }
#' @source Syntetyczne generowanie w R (DGP).
"smm_dane_surowe"

#' Dane eksperckie dla platform SMM
#'
#' Klasyczna macierz ocen eksperckich dla platform TikTok, Instagram i YouTube
#' pod kątem Zasięgu, Zaangażowania i Kosztów.
#'
#' @format Macierz o 3 wierszach i 3 kolumnach:
#' \describe{
#'   \item{Zasieg}{Ocena zasięgu}
#'   \item{Zaangazowanie}{Ocena zaangażowania}
#'   \item{Koszt}{Ocena kosztu}
#' }
#' @source Dane eksperckie z oryginalnego pakietu.
"smm_dane_eksperckie"
