source("src/installer.R")
source("src/utils.R")

install_over(
    "readxl", "dplyr", "purrr",
    "ggplot2", "gridExtra", "ggpubr",
    "tidyr"
)

ceramica <- read_excel("data/Dados de Exercícios/Ceramica/Ceramica.xlsx") %>%
    dplyr::select(-Amostra)

ceramica_medias <- ceramica %>%
    group_by(Sitio) %>%
    summarise(across(everything(), mean))

sitios <- ceramica %>%
    group_split(Sitio, .keep=FALSE) %>%
    setNames(unique(ceramica$Sitio))

ceramica_covs <- map(sitios, ~round(cov(.), 3))

ceramica_cors <- map(sitios, ~round(cor(.), 2))

ceramica %>%
    ggplot() +
    aes(sample = Sc) +
    geom_qq() +
    stat_qq() +
    stat_qq_line()

ceramica %>%
    ggplot() +
    aes(x = Sc, fill=Sitio) +
    geom_histogram()


map_elementos <- c(
    As = "Arsênio",
    Ce = "Cério",
    Cr = "Cromo",
    Eu = "Európio",
    Fe = "Ferro",
    Hf = "Háfnio",
    La = "Lantânio",
    Na = "Sódio",
    Nd = "Neodímio",
    Sc = "Escândio",
    Sm = "Samário",
    Th = "Tório",
    U = "Urânio"
)

plot_hist <- function(column, .df = "ceramica") {
    chart <- eval(parse(text=glue("
        ggplot({.df}) +
            aes(x = {column}, fill=Sitio) +
            geom_density() +
            theme_bw() +
            labs(x = map_elementos['{column}'], y = '', fill = 'Sitio') +
            scale_fill_viridis_d()
    ")))
    chart
}

ceramica_hists <- do.call(function(...) ggarrange(..., common.legend=TRUE, legend="bottom"), lapply(
    names(ceramica %>% select(-Sitio)),
    plot_hist
))

ggsave("img/ceramica_hists.pdf", ceramica_hists)



ceramica %>%
    group_by(Sitio) %>%
    summarise(across(everything(), list(
        ~ifelse(shapiro.test(.)$p.value < .05, "Sim", "Não")
    ))) %>%
    textbl(
        caption = "Normalidade univariada rejeitada (Shapiro-Wilk) nos elementos da cerâmica",
        label = "tab:normalidade_ceramica")


plot_qq <- function(column, .df = "ceramica") {
    xaxis_name <- map_elementos[coluna]
    eval(parse(text=glue("
        params <- list(mean = mean({.df}${column}), sd = sd({.df}${column}))

        ggplot({.df}) +
            aes(sample = {column}, color = Sitio)  +
            stat_qq(distribution = qnorm, dparams = params) +
            stat_qq_line(distribution = qnorm, dparams = params) +
            theme_bw() +
            labs(x = xaxis_name, y = '', color = 'Sitio') +
            scale_colour_viridis_d()
    ")))
}



plot_box <- function(column, .df = "ceramica") {
    eval(parse(text=glue("
        ggplot({.df}) +
            aes(y = {column}, fill=Sitio) +
            geom_boxplot() +
            theme_bw() +
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            labs(x = map_elementos['{column}'], y = '', fill = 'Sitio') +
            scale_fill_viridis_d()
    ")))
}

ceramica_boxes <- do.call(
    function(...) ggarrange(..., common.legend=TRUE, legend="bottom"),
    map(
        names(ceramica %>% select(-Sitio)),
        plot_box
    )
)

ggsave("img/ceramica_boxes.pdf", ceramica_boxes)
    

outliers <- ceramica %>%
    group_split(Sitio) %>%
    map(
        ~dplyr::filter(.,
        rowSums(across(where(is.numeric), is_outlier)) > 5
    )) %>%
    bind_rows()


renames <- paste(
    glue("{unname(map_elementos)} = {names(map_elementos)}"),
    collapse=", "
)
corrplot(
    eval(parse(text=glue("ceramica %>% select(-Sitio, {renames}) %>% cor"))),
    "img/elementos_corrplot.pdf"
)

ceramica_pcas <- princomp(ceramica %>% select(-Sitio), cor=TRUE)


comps_elementos <- with(ceramica_pcas, {
    cums <- cumsum(sdev ^ 2) / sum(sdev ^ 2)
    
    tibble(Componente = 1:length(sdev), Total = cums)
})

variabilidade_pcas <- function(pca_obj) {
    with(pca_obj, {
        eigenvals <- sdev ^ 2
        total <- sum(eigenvals)
        cumsum(eigenvals) / total
    })
}

comps_sitios <- map(sitios, ~princomp(., cor=TRUE) %>% variabilidade_pcas) %>%
    as_tibble() %>%
    mutate(Componente = 1:n()) %>%
    select(Componente, everything()) %>%
    left_join(comps_elementos) %>%
    textbl(
        caption = "Variabilidades explicadas por cada componente",
        label = "tab:pca_elementos_sitio"
    )

ceramica_pcas

with(ceramica_pcas, {
    l <- loadings

    sizes <- dim(l)

    matrix(as.numeric(l), nrow = sizes[1], ncol = sizes[2], dimnames = dimnames(l)) %>%
        as_tibble() %>%
        round(3) %>%
        mutate(Variavel = map_elementos[dimnames(l)[[1]]]) %>%
        select(Variavel, Comp.1, Comp.2, Comp.3) %>%
        textbl(
            caption = "Variabilidade de cada variável explicada pelas componentes",
            label = "tab:variaveis_explicacao_componentes_elementos"
        )
})
