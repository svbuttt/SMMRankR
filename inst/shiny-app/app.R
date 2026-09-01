# inst/shiny-app/app.R

library(shiny)
library(ggplot2)
library(ggrepel)

# Czyste R / WebAssembly - sprawdzamy obecnosc opcjonalnych pakietow.
#
# UWAGA (Shinylive/webR): requireNamespace() jest tam podmienione na wersje, ktora
# probuje doinstalowac brakujacy pakiet przez webr::install(). Gdy pakietu nie ma
# w repozytorium webR (np. SMMRankR, ktory nigdy tam nie trafi), shim zwraca wartosc
# o dlugosci zero, wiec `if (...)` przerywa start bledem "argument is of length zero"
# i aplikacja w ogole sie nie uruchamia. system.file() nie jest podmieniane,
# wiec uzywamy go jako bezpiecznego testu obecnosci pakietu.
pakiet_dostepny <- function(nazwa) {
  isTRUE(nzchar(system.file(package = nazwa)))
}

apa_html_dostepny <- pakiet_dostepny("flextable") && pakiet_dostepny("rempsyc")

if (apa_html_dostepny) {
  library(flextable)
  library(rempsyc)
}

# Sprawdzamy, czy SMMRankR jest zaladowany.
# W srodowisku Shinylive pliki R zostana wgrane obok app.R.
if (pakiet_dostepny("SMMRankR")) {
  library(SMMRankR)
  data("smm_dane_surowe", package = "SMMRankR", envir = environment())
} else {
  # Srodowisko WebAssembly/shinylive lub lokalny dev bez zainstalowanego pakietu
  # Najpierw szukamy w biezacym katalogu (dla Shinylive)
  r_files <- list.files(".", pattern = "\\.[Rr]$", full.names = TRUE)
  r_files <- r_files[basename(r_files) != "app.R"]
  
  # Jesli nie znaleziono, szukamy w ../../R (lokalny dev)
  if (length(r_files) == 0) {
    r_files <- list.files("../../R", full.names = TRUE, pattern = "\\.[Rr]$")
  }
  
  for (f in r_files) {
    source(f)
  }
  
  # Wczytujemy dane demonstracyjne
  if (file.exists("smm_dane_surowe.rds")) {
    smm_dane_surowe <- readRDS("smm_dane_surowe.rds")
  } else if (file.exists("../../data/smm_dane_surowe.rda")) {
    load("../../data/smm_dane_surowe.rda")
  }
}

# Lokalny parser do skladni lavaan w Shiny
analizuj_skladnia_smm_local <- function(skladnia) {
  skladnia_czysta <- gsub("\n", "", skladnia)
  linie <- strsplit(skladnia_czysta, ";")[[1]]
  struktura_kryteriow <- list()
  for (linia in linie) {
    if (trimws(linia) == "") 
      next
    czesci <- strsplit(linia, "=~")[[1]]
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      struktura_kryteriow[[nazwa_kryterium]] <- elementy
    }
  }
  return(struktura_kryteriow)
}

if (!exists("smm_dane_surowe")) {
  # Fallback na wypadek gdyby zbiór nie był wczytany
  smm_dane_surowe <- data.frame(
    Kampania_ID = 1:10,
    Platforma = rep(c("TikTok", "Instagram"), each = 5),
    zasieg_surowy = rnorm(10, 100, 10),
    liczba_klikniec = rnorm(10, 50, 5),
    koszt_kampanii = rnorm(10, 20, 2),
    polubienia = rnorm(10, 80, 8),
    komentarze = rnorm(10, 15, 2),
    udostepnienia = rnorm(10, 5, 1),
    wspolczynnik_konwersji = rnorm(10, 2, 0.2),
    cpc = rnorm(10, 0.4, 0.05),
    cpm = rnorm(10, 2, 0.2),
    satysfakcja_klienta = sample(1:5, 10, replace = TRUE),
    latwosc_obslugi = sample(1:5, 10, replace = TRUE),
    jakosc_wspolpracy = sample(1:5, 10, replace = TRUE)
  )
}

nazwy_platform <- unique(smm_dane_surowe$Platforma)

domyslna_skladnia <- "Zasieg =~ zasieg_surowy + liczba_klikniec;
Zaangazowanie =~ polubienia + komentarze + udostepnienia;
Koszty =~ koszt_kampanii + cpc + cpm;
Opinia =~ satysfakcja_klienta + latwosc_obslugi + jakosc_wspolpracy"

# --- INTERFEJS UŻYTKOWNIKA ---------------------------------------------------
ui <- tags$html(
  lang = "pl",
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("SMMRankR | Panel Analityczny MCDA"),
    # Tailwind CSS i Google Fonts
    tags$script(src = "https://cdn.tailwindcss.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&family=Outfit:wght@300;400;600;700;800&display=swap"),
    tags$style("
      body {
        font-family: 'Plus Jakarta Sans', sans-serif;
        background-color: #030712;
      }
      .font-outfit {
        font-family: 'Outfit', sans-serif;
      }
      /* Dostosowanie styli dla standardowych kontrolek Shiny */
      .shiny-input-container {
        margin-bottom: 0px !important;
      }
      .form-control, select, textarea {
        background-color: #1F2937 !important;
        color: #F9FAFB !important;
        border: 1px solid #374151 !important;
        border-radius: 0.375rem !important;
      }
      .form-control:focus, select:focus, textarea:focus {
        border-color: #06B6D4 !important;
        box-shadow: 0 0 0 2px rgba(6, 182, 212, 0.2) !important;
      }
      /* Custom scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      ::-webkit-scrollbar-track {
        background: #090d16;
      }
      ::-webkit-scrollbar-thumb {
        background: #1f2937;
        border-radius: 4px;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: #374151;
      }
    ")
  ),
  tags$body(
    class = "text-gray-200 min-h-screen flex flex-col",
    
    # 1. Nawigacja (Sticky Navbar)
    tags$nav(
      class = "sticky top-0 z-50 border-b border-gray-800 bg-gray-950/80 backdrop-blur-md",
      tags$div(
        class = "mx-auto max-w-7xl flex items-center justify-between px-4 py-3 sm:px-6 lg:px-8",
        tags$div(
          class = "flex items-center gap-3",
          tags$div(class = "flex h-9 w-9 items-center justify-center rounded border border-cyan-400 bg-cyan-400 font-black text-black font-outfit text-sm", "SR"),
          tags$div(
            tags$p(class = "text-sm font-bold uppercase tracking-[0.18em] text-white font-outfit", "SMMRankR"),
            tags$p(class = "text-[10px] text-cyan-400 font-medium", "MCDA w Wyborze Platform Social Media")
          )
        ),
        # Przyciski zakładek
        tags$div(
          class = "flex gap-2 text-xs font-semibold sm:text-sm",
          actionButton("tab_info", "Problem & Metody", class = "bg-transparent text-gray-300 hover:text-white px-3 py-1.5 border-0 rounded transition"),
          actionButton("tab_dane", "Eksploracja Danych", class = "bg-transparent text-gray-300 hover:text-white px-3 py-1.5 border-0 rounded transition"),
          actionButton("tab_analiza", "Panel Analiz", class = "bg-cyan-500 text-black px-4 py-1.5 border-0 rounded font-bold shadow-md shadow-cyan-500/20 transition hover:bg-cyan-400")
        )
      )
    ),
    
    # 2. Główna treść strony (Main Content)
    tags$main(
      class = "flex-grow mx-auto w-full max-w-7xl px-4 py-6 sm:px-6 lg:px-8",
      
      # Zakładka 1: PROBLEM & METODY
      conditionalPanel(
        condition = "output.current_tab == 'info'",
        tags$div(
          class = "grid gap-6 lg:grid-cols-[1.3fr_0.7fr]",
          # Karta opisu problemu
          tags$div(
            class = "rounded-xl border border-gray-800 bg-gray-900/40 p-6 shadow-xl",
            tags$p(class = "mb-2 text-xs font-bold uppercase tracking-[0.22em] text-cyan-400", "Kontekst Badawczy"),
            tags$h1(class = "text-3xl font-black tracking-tight text-white font-outfit sm:text-4xl", "Problem Wielokryterialny w SMM"),
            tags$p(
              class = "mt-4 text-gray-300 leading-relaxed text-sm sm:text-base",
              "Wybór optymalnej platformy Social Media Marketing (SMM) jest klasycznym problemem decyzyjnym. Menedżerowie marketingu muszą brać pod uwagę sprzeczne kryteria: chęć maksymalizacji zasięgu i zaangażowania użytkowników przy jednoczesnej dbałości o ograniczenie kosztów (budżet, CPC, CPM) oraz zachowanie wysokiej jakości współpracy i satysfakcji klienta."
            ),
            tags$p(
              class = "mt-3 text-gray-300 leading-relaxed text-sm sm:text-base",
              "Prezentowany pakiet rozszerza tradycyjne, uproszczone podejście (takie jak intuicyjne wagi eksperckie) na rzecz formalnego aparatu matematycznego wielokryterialnego wspomagania decyzji (MCDA). Pozwala to na obiektywizację decyzji biznesowych na podstawie rzeczywistych danych z kampanii marketingowych."
            ),
            # Siatka metod
            tags$div(
              class = "mt-6 grid gap-4 sm:grid-cols-2",
              tags$div(
                class = "rounded-lg border border-gray-800 bg-black/40 p-4",
                tags$p(class = "text-xs font-bold uppercase tracking-[0.18em] text-emerald-400 font-outfit", "1. Metody Ważenia Obiektywnego"),
                tags$p(class = "mt-2 text-xs text-gray-400 leading-relaxed", "Wagi kryteriów wyznaczane są na podstawie rozkładu i zmienności samych danych, eliminując subiektywizm decydenta. Implementujemy Entropię Shannona, metodę CRITIC oraz wagi Odchylenia Standardowego.")
              ),
              tags$div(
                class = "rounded-lg border border-gray-800 bg-black/40 p-4",
                tags$p(class = "text-xs font-bold uppercase tracking-[0.18em] text-purple-400 font-outfit", "2. Trzy Algorytmy MCDA"),
                tags$p(class = "mt-2 text-xs text-gray-400 leading-relaxed", "Porównanie alternatyw za pomocą trzech odmiennych podejść: TOPSIS (odległość geometryczna od ideału), VIKOR (indeks kompromisu i minimalizacja żalu) oraz WASPAS (połączenie sumy i iloczynu ważonego).")
              )
            )
          ),
          # Karta informatora akademickiego
          tags$div(
            class = "rounded-xl border border-gray-800 bg-gray-900/40 p-6 flex flex-col justify-between shadow-xl",
            tags$div(
              tags$h2(class = "text-lg font-bold text-white font-outfit", "Wymogi Licencjackie (EPI)"),
              tags$p(class = "mt-3 text-xs text-gray-400 leading-relaxed", "Projekt realizuje zaawansowane procedury przetwarzania informacji elektronicznej:"),
              tags$ul(
                class = "mt-4 space-y-2 text-xs text-gray-300 list-disc pl-4",
                tags$li("Dynamiczny parser składni podobnej do lavaan do agregacji wskaźników."),
                tags$li("Zaawansowane czyszczenie danych z obsługą kodów błędów i imputacją średnią grupową."),
                tags$li("Publikację tabel wynikowych bezpośrednio w standardzie APA (American Psychological Association)."),
                tags$li("Agregację rankingów (Meta-Ranking) za pomocą algorytmów konsensusu genetycznego i dominacji.")
              )
            ),
            tags$div(
              class = "mt-6 pt-4 border-t border-gray-800 text-xs text-gray-500",
              "SMMRankR Package - Promotor Branch 2026"
            )
          )
        )
      ),
      
      # Zakładka 2: EKSPLORACJA DANYCH
      conditionalPanel(
        condition = "output.current_tab == 'dane'",
        tags$div(
          class = "rounded-xl border border-gray-800 bg-gray-900/40 p-6 shadow-xl",
          tags$div(
            class = "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-6",
            tags$div(
              tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-emerald-400", "Zbiór testowy"),
              tags$h2(class = "text-2xl font-black text-white font-outfit", "Surowe dane kampanii marketingowych"),
              tags$p(class = "text-xs text-gray-400 mt-1", "Tabela prezentuje zbiór 'smm_dane_surowe' wygenerowany za pomocą DGP z losowymi szumami i brakami.")
            ),
            tags$div(
              class = "flex gap-2",
              selectInput("dane_platforma_filter", "Filtruj platformę:", choices = c("Wszystkie", nazwy_platform), selected = "Wszystkie", width = "180px")
            )
          ),
          
          # Tabela z surowymi danymi (z przewijaniem)
          tags$div(
            class = "overflow-x-auto rounded-lg border border-gray-800 bg-gray-950/60 max-h-[420px]",
            tableOutput("raw_data_table")
          )
        )
      ),
      
      # Zakładka 3: PANEL ANALIZ (Główna aplikacja)
      conditionalPanel(
        condition = "output.current_tab == 'analiza'",
        tags$div(
          class = "grid gap-6 lg:grid-cols-[340px_1fr]",
          
          # LEWY PANEL - KONFIGURACJA MODELU
          tags$div(
            class = "flex flex-col gap-5",
            
            # Karta parametrów
            tags$div(
              class = "rounded-xl border border-gray-800 bg-gray-900/40 p-5 shadow-lg",
              tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-cyan-400 mb-3", "Ustawienia Analizy"),
              
              # Metoda Wag
              tags$div(
                class = "mb-4",
                tags$label(class = "block text-xs font-bold text-gray-400 uppercase tracking-wider mb-2", "1. Metoda Wag Obiektywnych"),
                selectInput("wybór_wag", NULL, choices = c("Entropia Shannona" = "entropia", "Metoda CRITIC" = "critic", "Odchylenie Standardowe" = "std_dev"), selected = "entropia")
              ),
              
              # Metoda MCDA
              tags$div(
                class = "mb-4",
                tags$label(class = "block text-xs font-bold text-gray-400 uppercase tracking-wider mb-2", "2. Algorytm MCDA"),
                selectInput("wybór_mcda", NULL, choices = c("TOPSIS (Odległość od ideału)" = "topsis", "VIKOR (Kompromis)" = "vikor", "WASPAS (Suma i Iloczyn)" = "waspas", "META-RANKING (Konsensus)" = "meta"), selected = "meta")
              ),
              
              # Formuła lavaan
              tags$div(
                class = "mb-4",
                tags$label(class = "block text-xs font-bold text-gray-400 uppercase tracking-wider mb-2", "3. Składnia Agregacji (lavaan)"),
                textAreaInput("formuła_lavaan", NULL, value = domyslna_skladnia, rows = 6)
              ),
              
              # Przycisk obliczeń
              tags$div(
                class = "mt-6",
                actionButton("uruchom", "Uruchom analizę", class = "w-full bg-cyan-500 hover:bg-cyan-400 text-black font-black uppercase tracking-wider py-2.5 rounded-lg border-0 shadow-lg shadow-cyan-500/20 transition")
              )
            ),
            
            # Karta dynamicznych kierunków kryteriów
            tags$div(
              class = "rounded-xl border border-gray-800 bg-gray-900/40 p-5 shadow-lg",
              tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-emerald-400 mb-3", "Kierunki Optymalizacji"),
              tags$p(class = "text-[10px] text-gray-400 mb-4 leading-relaxed", "Określ, czy dane kryterium kompozytowe ma być maksymalizowane (zysk) czy minimalizowane (koszt). Wartości są generowane dynamicznie na podstawie składni powyżej."),
              uiOutput("dane_kierunki_ui")
            )
          ),
          
          # PRAWY PANEL - PREZENTACJA WYNIKÓW
          tags$div(
            class = "flex flex-col gap-6",
            
            # Wagi Kryteriów i Statystyka stabilności
            tags$div(
              class = "grid gap-6 md:grid-cols-2",
              
              # Karta Wag
              tags$div(
                class = "rounded-xl border border-gray-800 bg-gray-900/40 p-5 shadow-md flex flex-col justify-between",
                tags$div(
                  tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-cyan-400 mb-2", "Wagi Kryteriów"),
                  tags$h3(class = "text-lg font-bold text-white font-outfit", "Rozkład ważności czynników"),
                  tags$p(class = "text-xs text-gray-400 mt-1 leading-relaxed", "Wagi obliczone automatycznie wybraną metodą matematyczną na podstawie rozrzutu ocen w danych.")
                ),
                tags$div(
                  class = "h-48 mt-4 flex items-end",
                  plotOutput("plot_wagi", height = "100%")
                )
              ),
              
              # Karta Szybkiego Odczytu Rankingu
              tags$div(
                class = "rounded-xl border border-gray-800 bg-gray-900/40 p-5 shadow-md flex flex-col justify-between",
                tags$div(
                  tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-emerald-400 mb-2", "Lider Analizy"),
                  tags$h3(class = "text-lg font-bold text-white font-outfit", "Rekomendowana Platforma"),
                  tags$p(class = "text-xs text-gray-400 mt-1 leading-relaxed", "Alternatywa, która zajęła 1. miejsce w obliczeniach decyzyjnych.")
                ),
                tags$div(
                  class = "my-4 text-center",
                  uiOutput("lider_output")
                ),
                tags$div(
                  class = "text-xs text-gray-400 border-t border-gray-800 pt-3 flex justify-between",
                  tags$span("Zgodność metod (min. kor):"),
                  uiOutput("min_korelacja_ui")
                )
              )
            ),
            
            # Sformatowana tabela APA
            tags$div(
              class = "rounded-xl border border-gray-800 bg-gray-900/40 p-6 shadow-lg",
              tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-purple-400 mb-3", "Tabela Wynikowa APA"),
              tags$div(
                class = "overflow-x-auto p-4 bg-white rounded-lg flex justify-center",
                uiOutput("tabela_apa_output")
              )
            ),
            
            # Strategic Bubble Map (Wizualizacja)
            tags$div(
              class = "rounded-xl border border-gray-800 bg-gray-900/40 p-6 shadow-lg",
              tags$p(class = "text-xs font-bold uppercase tracking-[0.22em] text-amber-400 mb-3", "Wizualizacja Mapy Strategicznej"),
              tags$div(
                class = "h-[420px] bg-white rounded-lg p-3",
                plotOutput("plot_ranking", height = "100%")
              )
            )
          )
        )
      )
    ),
    
    # 3. Stopka strony
    tags$footer(
      class = "mt-auto border-t border-gray-800 bg-gray-950 py-4 text-center text-xs text-gray-500",
      "Praca Licencjacka EPI | SMMRankR Interactive Dashboard | Autor: Sofiia But"
    )
  )
)

# --- LOGIKA SERWERA (SERVER) -------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive służący do sterowania zakładkami w UI
  current_tab_val <- reactiveVal("info")
  
  observeEvent(input$tab_info, { current_tab_val("info") })
  observeEvent(input$tab_dane, { current_tab_val("dane") })
  observeEvent(input$tab_analiza, { current_tab_val("analiza") })
  
  output$current_tab <- reactive({
    current_tab_val()
  })
  outputOptions(output, "current_tab", suspendWhenHidden = FALSE)
  
  # --- FILTROWANIE SUROWYCH DANYCH -------------------------------------------
  output$raw_data_table <- renderTable({
    df <- smm_dane_surowe
    if (input$dane_platforma_filter != "Wszystkie") {
      df <- df[df$Platforma == input$dane_platforma_filter, ]
    }
    # Ograniczenie liczby wierszy do wyświetlenia dla wygody
    head(df, 30)
  }, striped = TRUE, spacing = "s", width = "100%", align = "c")
  
  # --- DYNAMICZNY PARSER KRYTERIÓW -------------------------------------------
  # Reaguje na edycję tekstu lavaan
  kryteria_list <- reactive({
    req(input$formuła_lavaan)
    tryCatch({
      # Wyciągamy nazwy kryteriów
      analizuj_skladnia_smm_local(input$formuła_lavaan)
    }, error = function(e) {
      list()
    })
  })
  
  # Generuje kontrolki wyboru kierunku (max/min) dla każdego kryterium
  output$dane_kierunki_ui <- renderUI({
    kryst_names <- names(kryteria_list())
    if (length(kryst_names) == 0) {
      return(tags$p(class = "text-red-400 text-xs", "Błąd: Brak poprawnych definicji kryteriów w składni."))
    }
    
    # Dla każdego kryterium tworzymy mały SelectInput w układzie poziomym
    control_list <- lapply(kryst_names, function(kname) {
      # Domyślnie koszty minimalizujemy, resztę maksymalizujemy
      sel_val <- if (grepl("koszt", kname, ignore.case = TRUE) || grepl("cena", kname, ignore.case = TRUE)) "min" else "max"
      
      tags$div(
        class = "flex items-center justify-between gap-3 mb-2 pb-2 border-b border-gray-800/60 last:border-b-0",
        tags$span(class = "text-xs font-medium text-gray-300", kname),
        selectInput(
          inputId = paste0("dir_", kname),
          label = NULL,
          choices = c("Maksymalizuj (max)" = "max", "Minimalizuj (min)" = "min"),
          selected = sel_val,
          width = "140px"
        )
      )
    })
    
    tags$div(control_list)
  })
  
  # --- OBLICZENIA I URUCHOMIENIE ANALIZY -------------------------------------
  obliczenia_wynik <- eventReactive(input$uruchom, {
    req(input$formuła_lavaan)
    
    # 1. Budowa macierzy decyzyjnej
    macierz <- built_matrix()
    req(macierz)
    
    # 2. Pobranie kierunków kryteriów z UI
    kryst_names <- names(kryteria_list())
    kierunki <- vapply(kryst_names, function(kname) {
      val <- input[[paste0("dir_", kname)]]
      if (is.null(val)) "max" else val
    }, character(1))
    
    # 3. Obliczenie wag
    wagi <- switch(
      input$wybór_wag,
      "entropia" = oblicz_wagi_entropia(macierz),
      "critic"   = oblicz_wagi_critic(macierz, kierunki),
      "std_dev"  = oblicz_wagi_std_dev(macierz, kierunki),
      oblicz_wagi_entropia(macierz)
    )
    
    # 4. Obliczenie rankingu MCDA
    mcda_res <- switch(
      input$wybór_mcda,
      "topsis" = oblicz_topsis_smm(macierz, wagi, kierunki),
      "vikor"  = oblicz_vikor_smm(macierz, wagi, kierunki, v = 0.5),
      "waspas" = oblicz_waspas_smm(macierz, wagi, kierunki, lambda = 0.5),
      "meta"   = oblicz_meta_ranking_smm(macierz, wagi, kierunki),
      oblicz_meta_ranking_smm(macierz, wagi, kierunki)
    )
    
    list(macierz = macierz, wagi = wagi, mcda = mcda_res, kierunki = kierunki)
  }, ignoreNULL = FALSE) # Odpalamy raz na starcie
  
  # Helper do budowania macierzy z obsługu błędów w formule
  built_matrix <- reactive({
    req(input$formuła_lavaan)
    tryCatch({
      zbuduj_macierz_decyzyjna(
        dane = smm_dane_surowe,
        skladnia = input$formuła_lavaan,
        kolumna_platformy = "Platforma",
        agregacja = mean
      )
    }, error = function(e) {
      showNotification(paste("Błąd budowania macierzy decyzyjnej:", e$message), type = "error")
      NULL
    })
  })
  
  # --- WYKRES WAG -------------------------------------------------------------
  output$plot_wagi <- renderPlot({
    res <- obliczenia_wynik()
    req(res)
    
    df_wagi <- data.frame(
      Kryterium = names(res$wagi),
      Waga = as.numeric(res$wagi),
      stringsAsFactors = FALSE
    )
    
    ggplot(df_wagi, aes(x = Kryterium, y = Waga, fill = Kryterium)) +
      geom_bar(stat = "identity", width = 0.55, color = "#0F172A", linewidth = 0.5) +
      scale_fill_manual(values = c("#06B6D4", "#10B981", "#F59E0B", "#8B5CF6", "#EC4899", "#3B82F6")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      geom_text(aes(label = sprintf("%.2f%%", Waga * 100)), vjust = -0.5, size = 3.5, fontface = "bold") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "#111827", color = NA),
        panel.background = element_rect(fill = "#111827", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#D1D5DB"),
        axis.text.x = element_text(color = "#F3F4F6", size = 11, face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "none"
      )
  }, bg = "#111827")
  
  # --- WYRENDEROWANIE TABELI APA ---------------------------------------------
  output$tabela_apa_output <- renderUI({
    res <- obliczenia_wynik()
    req(res)
    
    if (apa_html_dostepny) {
      ft <- tabela_apa(res$mcda)
      ft <- flextable::font(ft, fontname = "Plus Jakarta Sans", part = "all")
      ft <- flextable::fontsize(ft, size = 10, part = "all")
      ft <- flextable::autofit(ft)
      flextable::htmltools_value(ft)
    } else {
      # Fallback na czysty HTML (standardowe tagi htmltools)
      tabela_apa_html(res$mcda)
    }
  })
  
  # --- WYRENDEROWANIE WYKRESU RANKINGU ---------------------------------------
  output$plot_ranking <- renderPlot({
    res <- obliczenia_wynik()
    req(res)
    
    # Rysujemy mapę przy użyciu S3 plot z pakietu R
    # Dla meta-rankingu rysujemy porównanie słupkowe lub TOPSIS
    if (inherits(res$mcda, "meta_smm_wynik")) {
      # Dla meta-rankingu rysujemy podsumowanie pozycji alts
      df <- res$mcda$porownanie
      df_long <- data.frame(
        Platforma = rep(df$Platforma, 3),
        Metoda = rep(c("TOPSIS", "VIKOR", "WASPAS"), each = nrow(df)),
        Miejsce = c(df$Miejsce_TOPSIS, df$Miejsce_VIKOR, df$Miejsce_WASPAS),
        stringsAsFactors = FALSE
      )
      
      ggplot(df_long, aes(x = Platforma, y = Miejsce, fill = Metoda)) +
        geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
        scale_y_reverse(breaks = 1:nrow(df)) + # Odwracamy oś y, bo 1 to najlepsze miejsce
        scale_fill_manual(values = c("#06B6D4", "#10B981", "#3B82F6")) +
        labs(
          title = "Porównanie Pozycji Platform w Algorytmach",
          subtitle = "Im wyższy słupek (bliżej pozycji 1), tym lepsza ocena platformy",
          x = NULL, y = "Pozycja w rankingu"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#0F172A"),
          plot.subtitle = element_text(color = "#475569", size = 10, margin = margin(b = 10)),
          panel.border = element_rect(color = "#CBD5E1", fill = NA, linewidth = 0.5),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom"
        )
    } else {
      # TOPSIS, VIKOR lub WASPAS
      plot(res$mcda)
    }
  })
  
  # --- SZYBKI ODCZYT LIDERA I KORELACJI --------------------------------------
  output$lider_output <- renderUI({
    res <- obliczenia_wynik()
    req(res)
    
    best_name <- ""
    
    if (inherits(res$mcda, "meta_smm_wynik")) {
      best_name <- res$mcda$porownanie$Platforma[res$mcda$porownanie$Meta_Konsensus_RA == 1]
    } else {
      best_name <- res$mcda$ranking$Platforma[res$mcda$ranking$Pozycja_w_Rankingu == 1]
    }
    
    tags$div(
      tags$p(class = "text-5xl font-black text-cyan-400 font-outfit tracking-tight", best_name),
      tags$p(class = "text-xs text-gray-400 mt-2 font-medium", "Zwycięska platforma w analizie decyzyjnej")
    )
  })
  
  output$min_korelacja_ui <- renderUI({
    res <- obliczenia_wynik()
    req(res)
    
    if (inherits(res$mcda, "meta_smm_wynik")) {
      cor_mat <- res$mcda$zgodnosc_metod_korelacja
      cor_vals <- cor_mat[upper.tri(cor_mat)]
      min_cor <- min(cor_vals, na.rm = TRUE)
      
      text_color <- if (min_cor >= 0.8) "text-emerald-400" else if (min_cor >= 0.5) "text-amber-400" else "text-red-400"
      
      tags$span(class = paste("font-bold font-outfit", text_color), sprintf("%.2f", min_cor))
    } else {
      tags$span(class = "font-medium text-gray-500", "Niedostępne dla 1 metody")
    }
  })
}

# --- URUCHOMIENIE APLIKACJI --------------------------------------------------
shinyApp(ui = ui, server = server)
