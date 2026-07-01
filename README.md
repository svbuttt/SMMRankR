# SMMRankR: Pakiet R wspierający wybór platform do content marketingu w mediach społecznościowych

Pakiet `SMMRankR` implementuje zaawansowane procedury wielokryterialnego wspomagania decyzji (MCDA) w obszarze marketingu w mediach społecznościowych (SMM). Narzędzie pozwala na obiektywne porównanie i ranking platform (takich jak TikTok, Instagram, YouTube, Facebook, LinkedIn, Pinterest) na podstawie surowych danych z kampanii marketingowych przy użyciu wag obiektywnych oraz trzech klasycznych algorytmów decyzyjnych wraz z konsensusem (Meta-Rankingiem).

---

## 1. Metody Matematyczne (Wzory)

### A. Metody Wyznaczania Wag Obiektywnych

#### Entropia Shannona
Metoda mierzy stopień rozproszenia (niepewności) informacji w kryteriach. Im większa zmienność ocen w kryterium, tym większa waga obiektywna jest mu przypisywana.
1. Normalizacja sumacyjna ocen:
$$p_{ij} = \frac{x_{ij}}{\sum_{i=1}^{m} x_{ij}}$$
2. Obliczenie wskaźnika entropii dla każdego kryterium ($e_j$):
$$e_j = -k \sum_{i=1}^{m} p_{ij} \ln(p_{ij})$$
gdzie $k = \frac{1}{\ln(m)}$, a $m$ to liczba alternatyw.
3. Wyznaczenie stopnia rozbieżności ($d_j$) oraz ostatecznych wag ($w_j$):
$$d_j = 1 - e_j \quad \Longrightarrow \quad w_j = \frac{d_j}{\sum_{j=1}^{n} d_j}$$

#### Metoda CRITIC
Metoda uwzględnia zarówno kontrast intensywności (odchylenie standardowe) wewnątrz kryterium, jak i konflikt (korelację) między poszczególnymi kryteriami.
1. Normalizacja Min-Max ocen (w zależności od kierunku kryterium):
$$y_{ij} = \frac{x_{ij} - \min_i x_{ij}}{\max_i x_{ij} - \min_i x_{ij}} \quad (\text{dla max})$$
$$y_{ij} = \frac{\max_i x_{ij} - x_{ij}}{\max_i x_{ij} - \min_i x_{ij}} \quad (\text{dla min})$$
2. Obliczenie pojemności informacyjnej kryterium ($C_j$):
$$C_j = \sigma_j \sum_{k=1}^{n} (1 - r_{jk})$$
gdzie $\sigma_j$ to odchylenie standardowe kolumny, a $r_{jk}$ to współczynnik korelacji liniowej Pearsona pomiędzy kryterium $j$ i $k$.
3. Wyznaczenie wag ostatecznych ($w_j$):
$$w_j = \frac{C_j}{\sum_{k=1}^{n} C_k}$$

---

### B. Algorytmy Wielokryterialne (MCDA)

#### TOPSIS (Technique for Order of Preference by Similarity to Ideal Solution)
Metoda bazuje na geometrycznym wyborze alternatywy, która ma najmniejszą odległość od rozwiązania idealnego (PIS) i największą odległość od rozwiązania anty-idealnego (NIS).
1. Znormalizowana macierz ważona:
$$v_{ij} = w_j \cdot \frac{x_{ij}}{\sqrt{\sum_{i=1}^{m} x_{ij}^2}}$$
2. Wyznaczenie punktów odniesienia $v_j^+$ (PIS) oraz $v_j^-$ (NIS).
3. Obliczenie odległości euklidesowych:
$$d_i^+ = \sqrt{\sum_{j=1}^{n} (v_{ij} - v_j^+)^2}, \quad d_i^- = \sqrt{\sum_{j=1}^{n} (v_{ij} - v_j^-)^2}$$
4. Wyznaczenie współczynnika bliskości ($CC_i$):
$$CC_i = \frac{d_i^-}{d_i^+ + d_i^-}$$
Sortowanie platform następuje według malejącej wartości $CC_i$ (im wyższy współczynnik, tym lepsza alternatywa).

#### VIKOR (VlseKriterijumska Optimizacija I Kompromisno Resenje)
Metoda kompromisowa skupiająca się na maksymalizacji użyteczności grupowej oraz minimalizacji indywidualnego żalu.
1. Wyznaczenie grupowej użyteczności ($S_i$) i żalu ($R_i$):
$$S_i = \sum_{j=1}^{n} w_j \frac{f_j^* - x_{ij}}{f_j^* - f_j^-}, \quad R_i = \max_j \left[ w_j \frac{f_j^* - x_{ij}}{f_j^* - f_j^-} \right]$$
gdzie $f_j^*$ to wartość najlepsza, a $f_j^-$ to wartość najgorsza dla kryterium $j$.
2. Wyznaczenie ostatecznego indeksu kompromisu ($Q_i$):
$$Q_i = v \frac{S_i - S^*}{S^- - S^*} + (1 - v) \frac{R_i - R^*}{R^- - R^*}$$
gdzie $S^* = \min_i S_i$, $S^- = \max_i S_i$, $R^* = \min_i R_i$, $R^- = \max_i R_i$, a $v$ (domyślnie 0.5) to waga strategii większościowej.
Sortowanie platform następuje według rosnącej wartości $Q_i$ (im mniejszy indeks, tym lepsza pozycja).

#### WASPAS (Weighted Aggregated Sum Product Assessment)
Łączy addytywny model sumy ważonej (WSM) oraz multiplikatywny model iloczynu ważonego (WPM).
1. Część addytywna WSM ($Q_i^{(1)}$) i produktowa WPM ($Q_i^{(2)}$):
$$Q_i^{(1)} = \sum_{j=1}^{n} \bar{x}_{ij} w_j, \quad Q_i^{(2)} = \prod_{j=1}^{n} (\bar{x}_{ij})^{w_j}$$
gdzie $\bar{x}_{ij}$ to znormalizowana liniowo ocena kryterium.
2. Współczynnik kompromisowy WASPAS ($Q_i$):
$$Q_i = \lambda Q_i^{(1)} + (1 - \lambda) Q_i^{(2)}$$
Sortowanie platform następuje według malejącej wartości $Q_i$.

---

## 2. Instrukcja Używania Pakietu w R (Konsola)

### Instalacja i Załadowanie
```r
# Wymagane pakiety pomocnicze
install.packages(c("shiny", "ggplot2", "ggrepel", "rempsyc", "flextable", "RankAggreg"))

# Instalacja pakietu lokalnie lub z archiwum
devtools::install()
library(SMMRankR)
```

### Przykład Pełnej Analizy Konsolowej
```r
library(SMMRankR)

# 1. Załadowanie wbudowanych surowych danych kampanii społecznościowych
data("smm_dane_surowe")

# 2. Definiowanie modelu struktury w składni lavaan
formula_smm <- "
  Zasieg      =~ zasieg_surowy + liczba_klikniec;
  Zaangazowanie =~ polubienia + komentarze + udostepnienia;
  Koszty       =~ koszt_kampanii + cpc + cpm;
  Opinia       =~ satysfakcja_klienta + latwosc_obslugi + jakosc_wspolpracy
"

# 3. Budowa zagregowanej, czystej macierzy decyzyjnej
dec_matrix <- zbuduj_macierz_decyzyjna(
  dane = smm_dane_surowe,
  skladnia = formula_smm,
  kolumna_platformy = "Platforma",
  agregacja = mean
)

# 4. Wyznaczenie kierunków dla kryteriów (1-max, 0-min)
directions <- c("max", "max", "min", "max")

# 5. Obliczenie wag obiektywnych metodą CRITIC
wagi <- oblicz_wagi_critic(dec_matrix, directions)

# 6. Obliczenie ostatecznych rankingów decyzyjnych
wyniki_topsis <- oblicz_topsis_smm(dec_matrix, wagi, directions)
wyniki_meta <- oblicz_meta_ranking_smm(dec_matrix, wagi, directions)

# 7. Wygenerowanie tabeli w standardzie redakcyjnym APA
tabela_apa(wyniki_topsis, tytul = "Ocena platform za pomoca algorytmu TOPSIS")

# 8. Wygenerowanie wykresu strategicznego (mapy bąbelkowej)
plot(wyniki_topsis)
```

---

## 3. Interaktywna Aplikacja Shiny

Pakiet posiada zintegrowany, nowoczesny dashboard analityczny w trybie **Dark Mode** napisany przy użyciu biblioteki `Tailwind CSS`. 

### Jak uruchomić aplikację?
Aplikację uruchamia się bezpośrednio z poziomu konsoli R komendą:
```r
library(SMMRankR)
uruchom_aplikacje()
```

### Ograniczenia i Cel Projektu
> [!IMPORTANT]
> Aplikacja Shiny jest przeznaczona **wyłącznie do wizualizacji i obliczeń na bazie wbudowanych w pakiet danych demonstracyjnych** (`smm_dane_surowe` oraz `smm_dane_eksperckie`). Nie służy do wgrywania zewnętrznych plików użytkownika. Umożliwia natomiast pełne i dynamiczne modyfikowanie składni `lavaan` (regrupowanie zmiennych surowych w czynniki), zmianę wag, algorytmów decyzyjnych oraz kierunków optymalizacji kryteriów.

---

## 4. Autor
* **Sofiia But** (sofiia.but@student.uj.edu.pl)
* Uniwersytet Jagielloński w Krakowie
