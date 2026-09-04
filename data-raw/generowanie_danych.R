# data-raw/generowanie_danych.R

# Skrypt generuje syntetyczny, lecz wysoce realistyczny zbiór danych marketingowych
# dla platform społecznościowych (SMM), służący do testowania algorytmów MCDA.
# Zawiera celowo wprowadzone błędy (np. kod 999) oraz braki danych (NA) w celu przetestowania
# procedur czyszczenia.

set.seed(42)

liczba_platform <- 6
nazwy_platform <- c("TikTok", "Instagram", "YouTube", "Facebook", "LinkedIn", "Pinterest")
obserwacje_na_platforme <- 20
N <- liczba_platform * obserwacje_na_platforme

# --- PARAMETRY UKRYTE DLA PLATFORM (LATENT PROFILES) -------------------------
# Definiujemy średnie wartości cech dla każdej platformy (skala 0-1)
# Zasieg, Zaangazowanie, Koszty, Satysfakcja
parametry_platform <- list(
  TikTok = c(zasieg = 0.90, zaang = 0.85, koszt = 0.65, satysf = 0.70),
  Instagram = c(zasieg = 0.80, zaang = 0.90, koszt = 0.75, satysf = 0.85),
  YouTube = c(zasieg = 0.85, zaang = 0.70, koszt = 0.90, satysf = 0.80),
  Facebook = c(zasieg = 0.75, zaang = 0.40, koszt = 0.50, satysf = 0.60),
  LinkedIn = c(zasieg = 0.50, zaang = 0.65, koszt = 0.85, satysf = 0.75),
  Pinterest = c(zasieg = 0.40, zaang = 0.35, koszt = 0.30, satysf = 0.55)
)

# --- FUNKCJE POMOCNICZE DGP --------------------------------------------------
rhalf_normal <- function(n, sd = 1) {
  abs(stats::rnorm(n, mean = 0, sd = sd))
}

ogranicz <- function(x, dol, gora) {
  pmin(pmax(x, dol), gora)
}

# Symulacja skali Likerta za pomocą rozkładu Beta
likert_beta <- function(latent, min_wynik = 1, max_wynik = 5, koncentracja = 15) {
  latent <- ogranicz(latent, 0.05, 0.95)
  surowe <- stats::rbeta(
    n = length(latent),
    shape1 = latent * koncentracja,
    shape2 = (1 - latent) * koncentracja
  )
  as.integer(round(min_wynik + surowe * (max_wynik - min_wynik)))
}

# --- GENEROWANIE RAMKI DANYCH ------------------------------------------------
platformy_wektor <- rep(nazwy_platform, each = obserwacje_na_platforme)

# Inicjalizacja list kolumn
zasieg_surowy <- numeric(N)
liczba_klikniec <- integer(N)
wspolczynnik_konwersji <- numeric(N)
polubienia <- integer(N)
komentarze <- integer(N)
udostepnienia <- integer(N)
koszt_kampanii <- numeric(N)
satysfakcja_klienta <- integer(N)
latwosc_obslugi <- integer(N)
jakosc_wspolpracy <- integer(N)

for (i in seq_along(platformy_wektor)) {
  plat <- platformy_wektor[i]
  params <- parametry_platform[[plat]]
  
  # Szum specyficzny dla obserwacji
  szum <- stats::rnorm(1, mean = 0, sd = 0.08)
  
  # 1. ZASIĘG: Rozkład log-normalny, zależny od latent_zasieg
  mu_zasieg <- params["zasieg"] * 8.0 + 3.0 # e.g. ok. e^10 views
  zasieg_surowy[i] <- round(stats::rlnorm(1, meanlog = mu_zasieg + szum, sdlog = 0.25), 0)
  
  # 2. ZAANGAŻOWANIE:
  # Clicki zależne od zasięgu i współczynnika klikalności (CTR)
  ctr_mu <- params["zaang"] * 0.04 + 0.01 # 1% - 5%
  ctr <- ogranicz(stats::rbeta(1, shape1 = ctr_mu * 50, shape2 = (1 - ctr_mu) * 50), 0.005, 0.15)
  liczba_klikniec[i] <- as.integer(round(zasieg_surowy[i] * ctr, 0))
  
  # Współczynnik konwersji (CR) zależny od zaangażowania
  cr_mu <- params["zaang"] * 0.025 + 0.005 # 0.5% - 3%
  cr <- ogranicz(stats::rbeta(1, shape1 = cr_mu * 80, shape2 = (1 - cr_mu) * 80), 0.001, 0.08)
  wspolczynnik_konwersji[i] <- round(cr * 100, 2)
  
  # Reakcje społecznościowe (Polubienia, Komentarze, Udostępnienia)
  er_mu <- params["zaang"] * 0.06 + 0.02 # 2% - 8%
  reakcje <- zasieg_surowy[i] * ogranicz(stats::rnorm(1, mean = er_mu, sd = 0.01), 0.005, 0.20)
  
  polubienia[i] <- as.integer(round(reakcje * 0.80, 0))
  komentarze[i] <- as.integer(round(reakcje * 0.15, 0))
  udostepnienia[i] <- as.integer(round(reakcje * 0.05, 0))
  
  # 3. KOSZTY:
  # Koszt kampanii zależny od koszt_latentny i zasięgu
  mu_koszt <- params["koszt"] * 7.5 + 4.5
  koszt_kampanii[i] <- round(stats::rlnorm(1, meanlog = mu_koszt + szum, sdlog = 0.20), 2)
  
  # 4. SATYSFAKCJA (Likert 1-5)
  satysfakcja_klienta[i] <- likert_beta(params["satysf"] + szum, 1, 5)
  latwosc_obslugi[i] <- likert_beta(params["satysf"] * 0.9 + 0.05 + szum, 1, 5)
  jakosc_wspolpracy[i] <- likert_beta(params["satysf"] * 1.1 - 0.05 + szum, 1, 5)
}

# Obliczamy CPC i CPM na bazie wygenerowanych wartości
cpc <- round(koszt_kampanii / (liczba_klikniec + 1), 2)
cpm <- round((koszt_kampanii / (zasieg_surowy + 1)) * 1000, 2)

# --- WPROWADZENIE KONTROLOWANYCH BŁĘDÓW I BRAKÓW DANYCH ----------------------
# 1. Kody błędów 999 (np. systemowe błędy zapisu wartości)
indeksy_bledow_sat <- sample(1:N, 4)
satysfakcja_klienta[indeksy_bledow_sat] <- 999

indeksy_bledow_klik <- sample(1:N, 2)
liczba_klikniec[indeksy_bledow_klik] <- 999

# 2. Braki danych NA (np. brak wypełnienia sekcji ankiety lub błąd API)
indeksy_na_wspolpraca <- sample(1:N, 6)
jakosc_wspolpracy[indeksy_na_wspolpraca] <- NA_integer_

indeksy_na_konwersja <- sample(1:N, 5)
wspolczynnik_konwersji[indeksy_na_konwersja] <- NA_real_

# --- ZŁOŻENIE RAMKI DANYCH ---------------------------------------------------
smm_dane_surowe <- data.frame(
  Kampania_ID = 1:N,
  Platforma = platformy_wektor,
  zasieg_surowy = zasieg_surowy,
  liczba_klikniec = liczba_klikniec,
  wspolczynnik_konwersji = wspolczynnik_konwersji,
  polubienia = polubienia,
  komentarze = komentarze,
  udostepnienia = udostepnienia,
  koszt_kampanii = koszt_kampanii,
  cpc = cpc,
  cpm = cpm,
  satysfakcja_klienta = satysfakcja_klienta,
  latwosc_obslugi = latwosc_obslugi,
  jakosc_wspolpracy = jakosc_wspolpracy,
  stringsAsFactors = FALSE
)

# --- WALIDACJA JAKOŚCI GENEROWANIA -------------------------------------------
if (nrow(smm_dane_surowe) != N) {
  stop("Błąd walidacji: Niepoprawna liczba wierszy.")
}
if (length(unique(smm_dane_surowe$Platforma)) != liczba_platform) {
  stop("Błąd walidacji: Niepoprawna liczba unikalnych platform.")
}
if (any(smm_dane_surowe$zasieg_surowy < 0)) {
  stop("Błąd walidacji: Wykryto ujemne wartości zasięgu.")
}

# --- ZAPIS DANYCH W PAKIECIE -------------------------------------------------
# Zapisujemy do folderu data/
usethis::use_data(smm_dane_surowe, overwrite = TRUE)

message("Pomyślnie wygenerowano i zapisano zbiór smm_dane_surowe!")
