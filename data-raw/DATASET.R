smm_dane_eksperckie <- matrix(c(90, 80, 30, 75, 95, 60, 65, 70, 45), nrow = 3, byrow = TRUE)
colnames(smm_dane_eksperckie) <- c("Zasieg", "Zaangazowanie", "Koszt")
rownames(smm_dane_eksperckie) <- c("TikTok", "Instagram", "YouTube")
usethis::use_data(smm_dane_eksperckie, overwrite = TRUE)
