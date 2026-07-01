# tools/export_shinylive.R

# Skrypt służy do eksportu aplikacji Shiny do postaci statycznych plików WebAssembly (shinylive).
# Wygenerowane pliki zostaną zapisane w folderze `docs/` na branchu `promotor`.
# Pozwala to na uruchomienie w pełni interaktywnej aplikacji Shiny bezpośrednio na GitHub Pages.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  message("Instalacja pakietu 'shinylive'...")
  install.packages("shinylive", repos = "https://cloud.r-project.org")
}

# Sprawdzamy czy istnieje katalog aplikacji
app_dir <- "inst/shiny-app"
if (!dir.exists(app_dir)) {
  stop("Nie znaleziono katalogu aplikacji 'inst/shiny-app'!")
}

# Usuwamy stary katalog docs/ jeśli istnieje w celu czystej instalacji
dest_dir <- "docs"
if (dir.exists(dest_dir)) {
  message("Usuwanie starego katalogu docs/...")
  unlink(dest_dir, recursive = TRUE, force = TRUE)
}

message("Eksport aplikacji Shiny za pomocą shinylive...")
shinylive::export(appdir = app_dir, destdir = dest_dir)

# Tworzymy plik index.html w katalogu głównym jako przekierowanie/furtka (opcjonalnie)
# Często GitHub Pages serwuje z folderu docs/, wtedy index.html w docs/ jest główną stroną.
# Jeśli użytkownik włączy GH Pages na katalog główny, index.html w katalogu głównym może przekierować do docs/
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
