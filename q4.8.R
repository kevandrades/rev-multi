source("src/installer.R")
source("src/utils.R")
options(OutDec = ",")

install_over("dplyr", "readxl", "misty", "psych", "ggplot2")

stress <- read_excel("data/Dados de Exercícios/Stress/Stress.xlsx") %>%
    select(X1:X14)

poly_stress <- with(cor.matrix(stress, method="poly"), result$cor)

with(
    eigen(poly_stress), data.frame(
        Componente = factor(
            glue::glue("X{1:length(values)}"),
            levels=glue::glue("X{1:length(values)}")
        ),
        Variabilidade = (values / sum(values)) %>% round(3)
    ) %>%
    ggplot() +
    aes(x = Componente, y = Variabilidade, fill=Componente) +
    geom_col() +
    scale_fill_viridis_d() +
    guides(fill=F) +
    theme_bw()
)

ggsave("img/stress_variabilidade.pdf", width=10, height=5, scale=.7)

stress_fact <- principal(r=poly_stress, nfactors=5, rotate="varimax")

stress_fact_df <- with(stress_fact, {
    matrix(as.numeric(loadings), dim(loadings), dimnames = dimnames(loadings)) %>%
        as_tibble() %>%
        mutate(
            Comunalidade = communality,
            Variavel = 1:n()
        ) %>%
        select(Variavel, RC1, RC2, RC3, RC4, RC5, Comunalidade)
})

textbl(stress_fact_df, digits=3, caption="Análise fatorial do questionário sobre Stress", label="tab:stress_fact")


stress_x <- c("Necessidade de sempre jogar bem", "Perder",
"Autocobrança exagerada", "Pensamentos negativos sobre sua carreira",
"Perder o jogo praticamente ganho",
"Repetir os mesmos erros",
"Cometer erros que provocam a derrota da equipe",
"Adversário desleal",
"Arbitragem prejudica você",
"Falta de humildade de um companheiro de equipe",
"Pessoas com pensamento negativo",
"Companheiro desleal",
"Diferenças de tratamento na equipe",
"Falta de confiança por parte do técnico")

stress_fact_df <- stress_fact_df %>% mutate(Questao = stress_x[Variavel])
stress_fact_df %>% arrange(-abs(RC2))
stress_fact_df %>% arrange(-abs(RC3))
stress_fact_df %>% arrange(-abs(RC4))
stress_fact_df %>% arrange(-abs(RC5))
