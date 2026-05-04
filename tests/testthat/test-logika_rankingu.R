test_that("Ranking TOPSIS zwraca poprawne wymiary", {
  dane <- stworz_macierz_smm()
  wagi <- oblicz_wagi_smm_bwm(1, 3, 3)
  wynik <- oblicz_topsis_smm(dane, wagi)


  expect_equal(nrow(wynik), 3)

  expect_true(all(wynik$Ocena >= 0 & wynik$Ocena <= 1))
})
