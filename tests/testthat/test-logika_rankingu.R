test_that("Rankingi MCDA zwracają poprawne wyniki i wymiary", {
  dane <- stworz_macierz_smm()
  wagi <- oblicz_wagi_entropia(dane)
  
  # Przetestowanie TOPSIS
  wynik_topsis <- oblicz_topsis_smm(dane, wagi)
  expect_equal(nrow(wynik_topsis$ranking), 3)
  expect_true(all(wynik_topsis$ranking$Wskaznik_CC >= 0 & wynik_topsis$ranking$Wskaznik_CC <= 1))
  
  # Przetestowanie VIKOR
  wynik_vikor <- oblicz_vikor_smm(dane, wagi)
  expect_equal(nrow(wynik_vikor$ranking), 3)
  expect_true(all(wynik_vikor$ranking$Indeks_Q >= 0 & wynik_vikor$ranking$Indeks_Q <= 1))
  
  # Przetestowanie WASPAS
  wynik_waspas <- oblicz_waspas_smm(dane, wagi)
  expect_equal(nrow(wynik_waspas$ranking), 3)
  expect_true(all(wynik_waspas$ranking$Wskaznik_Q_WASPAS >= 0 & wynik_waspas$ranking$Wskaznik_Q_WASPAS <= 1))
})
