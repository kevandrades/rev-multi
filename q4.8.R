source("src/installer.R")

install_over("dplyr", "readxl", "misty", "psych")

stress <- read_excel("data/Dados de Exercícios/Stress/Stress.xlsx") %>%
    select(X1:X14)

poly_stress <- with(cor.matrix(stress, method="poly"), result$cor)


with(eigen(poly_stress), cumsum(values / sum(values)))

principal(r=poly_stress, nfactors=5, rotate="varimax")

