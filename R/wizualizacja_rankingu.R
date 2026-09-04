#' @title Wewnetrzny motyw graficzny dla wykresow SMM
#' @description Ujednolicony, minimalistyczny styl wykresow dla calego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_smm <- function() {
  list(
    theme_minimal(base_size = 11),
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#0F172A"),
      plot.subtitle = element_text(color = "#475569", size = 10, margin = margin(b = 10)),
      panel.border = element_rect(color = "#CBD5E1", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "#F1F5F9"),
      panel.grid.minor = element_line(color = "#F8FAFC"),
      legend.position = "right",
      axis.title = element_text(face = "bold", size = 10, color = "#1E293B"),
      axis.text = element_text(color = "#475569")
    )
  )
}

#' Mapa Efektywnosci TOPSIS
#'
#' @description Wizualizuje odleglosci alternatyw od idealu i anty-idealu.
#' Os X: Dystans od Najgorszego (D-). Os Y: Dystans do Najlepszego (D+).
#' Najlepsze platformy znajduja sie w prawym dolnym rogu.
#'
#' @param x Obiekt klasy \code{topsis_smm_wynik}.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.topsis_smm_wynik <- function(x, ...) {
  df <- x$ranking
  # Przeskalowanie wskaznika CC do rozmiaru babla
  df$Rozmiar <- df$Wskaznik_CC^4
  
  cel_x <- max(df$Dystans_Od_Antyidealu) * 1.05
  cel_y <- min(df$Dystans_Od_Idealu) * 0.95
  
  # Wizualna odleglosc euklidesowa do punktu idealnego
  df$OdlegloscWizualna <- sqrt((df$Dystans_Od_Antyidealu - cel_x)^2 + (df$Dystans_Od_Idealu - cel_y)^2)
  
  ggplot(df, aes(x = Dystans_Od_Antyidealu, y = Dystans_Od_Idealu)) +
    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "#64748B") +
    geom_label(aes(x = (Dystans_Od_Antyidealu + cel_x) / 2, y = (Dystans_Od_Idealu + cel_y) / 2,
                   label = sprintf("%.2f", OdlegloscWizualna)),
               size = 2.5, color = "#475569", fill = "#FFFFFF", label.size = 0, alpha = 0.7) +
    geom_point(aes(size = Rozmiar, fill = Wskaznik_CC), shape = 21, color = "#0F172A", alpha = 0.9) +
    geom_text_repel(aes(label = Platforma), box.padding = 0.5, fontface = "bold") +
    annotate("point", x = cel_x, y = cel_y, shape = 18, size = 6, color = "#F59E0B") +
    annotate("text", x = cel_x, y = cel_y, label = "PUNKT IDEALNY", vjust = 2, size = 3, fontface = "bold", color = "#B45309") +
    scale_fill_gradient(low = "#94A3B8", high = "#06B6D4") + # Od szarego do jasnego cyjanu
    scale_size_continuous(range = c(4, 12), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.18))) +
    scale_y_continuous(expand = expansion(mult = c(0.18, 0.08))) +
    labs(
      title = "Mapa Odleglosci od Idealu (TOPSIS)",
      subtitle = "Cel: Prawy-dolny rog (najblizej punktu idealnego)",
      x = "Dystans od Platformy Najgorszej (D-)",
      y = "Dystans do Platformy Najlepszej (D+)",
      fill = "Wskaznik CC"
    ) +
    .motyw_smm()
}

#' Mapa Strategiczna VIKOR
#'
#' @description Pokazuje kompromis miedzy maksymalizacja uzytecznosci grupowej (os X)
#' a minimalizacja indywidualnego zalu (os Y).
#'
#' @param x Obiekt klasy \code{vikor_smm_wynik}.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.vikor_smm_wynik <- function(x, ...) {
  df <- x$ranking
  
  # Przeskalowanie S dla osi X (Wydajnosc: odwrocone S do 0-100)
  s_min <- min(df$Wskaznik_S)
  s_max <- max(df$Wskaznik_S)
  mianownik_S <- s_max - s_min
  if (mianownik_S == 0) mianownik_S <- 1
  df$Wydajnosc <- ((s_max - df$Wskaznik_S) / mianownik_S) * 100
  
  # Rozmiar babla (odwrocone Q - im mniejsze Q, tym wiekszy babel)
  q_min <- min(df$Indeks_Q)
  q_max <- max(df$Indeks_Q)
  mianownik_Q <- q_max - q_min
  if (mianownik_Q == 0) mianownik_Q <- 1
  q_inv <- 1.0 - ((df$Indeks_Q - q_min) / mianownik_Q)
  df$Rozmiar <- (q_inv + 0.1)^2
  
  # Wyznaczenie punktow odniesienia (kwadranty)
  srodek_perf <- stats::median(df$Wydajnosc, na.rm = TRUE)
  srodek_ryzyko <- stats::median(df$Wskaznik_R, na.rm = TRUE)
  
  ggplot(df, aes(x = Wydajnosc, y = Wskaznik_R)) +
    annotate("rect", xmin = srodek_perf, xmax = Inf, ymin = -Inf, ymax = srodek_ryzyko, fill = "#ECFDF5", alpha = 0.6) +
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "#94A3B8") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "#94A3B8") +
    annotate("text", x = max(df$Wydajnosc), y = min(df$Wskaznik_R), label = "STREFA LIDERA\n(Wysoka Wydajnosc, Niskie Ryzyko)", 
             hjust = 1, vjust = 0, size = 2.5, fontface = "bold.italic", color = "#047857") +
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "#0F172A", alpha = 0.8) +
    geom_text_repel(aes(label = Platforma), box.padding = 0.5, fontface = "bold") +
    scale_fill_gradient(low = "#94A3B8", high = "#10B981") + # Od szarego do szmaragdowej zieleni
    scale_size_continuous(range = c(4, 12), guide = "none") +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    labs(
      title = "Mapa Strategiczna VIKOR",
      subtitle = "Zielona cwiartka reprezentuje optymalne rozwiazanie kompromisowe",
      x = "Indeks Wydajnosci Grupowej (S odwrocone, 0-100)",
      y = "Indeks Indywidualnego Zalu (R, ryzyko)",
      fill = "Wydajnosc"
    ) +
    .motyw_smm()
}

#' Mapa Balansu WASPAS
#'
#' @description Prezentuje relacje miedzy wynikiem sumarycznym (WSM) a iloczynowym (WPM).
#' Najlepsze platformy leza w prawym gornym rogu.
#'
#' @param x Obiekt klasy \code{waspas_smm_wynik}.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.waspas_smm_wynik <- function(x, ...) {
  df <- x$ranking
  df$Rozmiar <- df$Wskaznik_Q_WASPAS^4
  
  srodek_wsm <- stats::median(df$Wynik_WSM, na.rm = TRUE)
  srodek_wpm <- stats::median(df$Wynik_WPM, na.rm = TRUE)
  
  ggplot(df, aes(x = Wynik_WSM, y = Wynik_WPM)) +
    annotate("rect", xmin = srodek_wsm, xmax = Inf, ymin = srodek_wpm, ymax = Inf, fill = "#EFF6FF", alpha = 0.6) +
    geom_vline(xintercept = srodek_wsm, linetype = "dashed", color = "#94A3B8") +
    geom_hline(yintercept = srodek_wpm, linetype = "dashed", color = "#94A3B8") +
    annotate("text", x = max(df$Wynik_WSM), y = max(df$Wynik_WPM), label = "PODWOJNA PRZEWAGA\n(Mocne WSM i WPM)",
             hjust = 1, vjust = 1, size = 2.5, fontface = "bold.italic", color = "#1E402B") +
    geom_point(aes(size = Rozmiar, fill = Wskaznik_Q_WASPAS), shape = 21, color = "#0F172A", alpha = 0.8) +
    geom_text_repel(aes(label = Platforma), box.padding = 0.5, fontface = "bold") +
    scale_fill_gradient(low = "#94A3B8", high = "#3B82F6") + # Od szarego do niebieskiego
    scale_size_continuous(range = c(4, 12), guide = "none") +
    labs(
      title = "Mapa Balansu WASPAS",
      subtitle = "Prawy-gorny rog reprezentuje wysokie wartosci WSM i WPM",
      x = "Model Sumaryczny (WSM)",
      y = "Model Multiplikatywny (WPM)",
      fill = "Ocena Q"
    ) +
    .motyw_smm()
}

#' Rysowanie rankingu SMM (funkcja kompatybilnosci)
#'
#' @description Tradycyjny rysunek zachowany dla kompatybilnosci wstecznej, wykorzystujacy
#' nowa logike graficzna ggplot2.
#' @param macierz_danych Macierz z danymi.
#' @param wyniki_rankingu Wyniki rankingu (np. z TOPSIS).
#' @export
rysuj_ranking_smm <- function(macierz_danych, wyniki_rankingu) {
  # Jesli wyniki_rankingu to obiekt klasy topsis_smm_wynik, rysujemy go metoda S3 plot
  if (inherits(wyniki_rankingu, "topsis_smm_wynik")) {
    print(plot(wyniki_rankingu))
  } else if (is.data.frame(wyniki_rankingu) && "Ocena" %in% names(wyniki_rankingu)) {
    # Klasyczna prosta wizualizacja
    df <- data.frame(
      Platforma = wyniki_rankingu$Platforma,
      Ocena = wyniki_rankingu$Ocena,
      Zasieg = macierz_danych[, "Zasieg"],
      stringsAsFactors = FALSE
    )
    p <- ggplot(df, aes(x = Zasieg, y = Ocena, label = Platforma)) +
      geom_point(size = 6, color = "skyblue") +
      geom_text_repel() +
      labs(title = "Wykres Kompatybilnosci SMM", x = "Zasieg", y = "Ocena TOPSIS") +
      theme_minimal()
    print(p)
  } else {
    # Fallback
    graphics::plot(macierz_danych[,1], macierz_danych[,2], main = "Mapa SMM")
  }
}

# Definiowanie globalnych zmiennych w celu przejscia testow R CMD check
utils::globalVariables(c("Dystans_Od_Antyidealu", "Dystans_Od_Idealu", "OdlegloscWizualna", 
                          "Rozmiar", "Wskaznik_CC", "Platforma", "Wydajnosc", "Wskaznik_R", 
                          "Wynik_WSM", "Wynik_WPM", "Wskaznik_Q_WASPAS", "Zasieg", "Ocena"))
