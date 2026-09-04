# tools/export_shinylive.R

# Skrypt służy do eksportu aplikacji Shiny do postaci statycznych plików WebAssembly (shinylive).
# Wygenerowane pliki zostaną zapisane w folderze `docs/` na branchu `promotor`.
# Pozwala to na uruchomienie w pełni interaktywnej aplikacji Shiny bezpośrednio na GitHub Pages.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  message("Instalacja pakietu 'shinylive'...")
  install.packages("shinylive", repos = "https://cloud.r-project.org")
}

# Sprawdzamy czy istnieje katalog aplikacji
if (!dir.exists("inst/shiny-app")) {
  stop("Nie znaleziono katalogu aplikacji 'inst/shiny-app'!")
}

# Tworzymy tymczasowy katalog do skompilowania samowystarczalnej aplikacji
temp_app_dir <- tempfile("smmrankr-shinylive-")
dir.create(temp_app_dir, recursive = TRUE)
message("Tworzenie samowystarczalnego katalogu eksportu: ", temp_app_dir)

# 1. Kopiowanie app.R
file.copy("inst/shiny-app/app.R", file.path(temp_app_dir, "app.R"))

# 2. Kopiowanie skryptow pakietu R/ (ktore aplikacja source-uje w WebAssembly)
r_files_to_copy <- c(
  "R/algorytm_mcda.R",
  "R/metody_wazenia.R",
  "R/przygotowanie_danych.R",
  "R/tabela_apa.R",
  "R/wizualizacja_rankingu.R",
  "R/dane_marketingowe.R",
  "R/data.R"
)
for (f in r_files_to_copy) {
  if (file.exists(f)) {
    file.copy(f, file.path(temp_app_dir, basename(f)))
  }
}

# 3. Zapisanie bazy danych surowych do pliku RDS w katalogu aplikacji
if (file.exists("data/smm_dane_surowe.rda")) {
  env <- new.env()
  load("data/smm_dane_surowe.rda", envir = env)
  if (exists("smm_dane_surowe", envir = env)) {
    saveRDS(env$smm_dane_surowe, file.path(temp_app_dir, "smm_dane_surowe.rds"))
  }
}

# Usuwamy stary katalog docs/ jeśli istnieje w celu czystej instalacji
dest_dir <- "docs"
if (dir.exists(dest_dir)) {
  message("Usuwanie starego katalogu docs/...")
  unlink(dest_dir, recursive = TRUE, force = TRUE)
}

message("Eksport aplikacji Shiny za pomocą shinylive...")
shinylive::export(appdir = temp_app_dir, destdir = dest_dir)

# Czyszczenie tymczasowego katalogu
unlink(temp_app_dir, recursive = TRUE, force = TRUE)

# Tworzymy plik index.html w katalogu głównym jako przekierowanie/furtka (opcjonalnie)
if (!file.exists("index.html")) {
  message("Tworzenie index.html w katalogu głównym (furtka do docs)...")
  redirect_html <- '<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="refresh" content="0; url=docs/index.html" />
  <title>Przekierowanie do Aplikacji Shiny</title>
</head>
<body>
  <p>Jeśli nie zostałeś przekierowany automatycznie, <a href="docs/index.html">kliknij tutaj</a>.</p>
</body>
</html>'
  writeLines(redirect_html, "index.html")
}

message("Eksport zakończony sukcesem! Folder 'docs/' zawiera gotową statyczną aplikację.")
message("Aby wdrożyć ją na GitHub Pages:")
message("1. Wyślij zmiany na GitHub (branch 'promotor').")
message("2. W ustawieniach repozytorium -> Pages, wybierz źródło: 'Deploy from a branch' i folder '/docs'.")
