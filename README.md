# SMMRankR

Pakiet SMMRankR służy do wielokryterialnego wspomagania decyzji (MCDA) w obszarze marketingu w mediach społecznościowych. Pozwala na obiektywne porównanie platform takich jak TikTok, Instagram czy YouTube.

## O projekcie
Projekt implementuje dwie kluczowe metody matematyczne:
* Best-Worst Method (BWM) – do wyznaczania wag kryteriów.
* TOPSIS – do tworzenia końcowego rankingu platform.

## Funkcje pakietu
* stworz_macierz_smm() – generuje dane do analizy.
* oblicz_wagi_smm_bwm() – oblicza ważność kryteriów.
* oblicz_topsis_smm() – tworzy ranking końcowy.
* rysuj_ranking_smm() – generuje wykres bąbelkowy (mapę strategiczną).

## Przykład użycia
library(SMMRankR)
dane <- stworz_macierz_smm()
wagi <- oblicz_wagi_smm_bwm(1, 3, 3)
wynik <- oblicz_topsis_smm(dane, wagi)
rysuj_ranking_smm(dane, wynik)

## Autor
Sofiia But
