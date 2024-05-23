if (!require(glue)) install.packages("glue")

install_over <- function(..., repos = "https://vps.fmvz.usp.br/CRAN/") {
    packages <- unlist(list(...))

    statement <- paste(
        glue::glue("
            if (!require({packages})) install.packages('{packages}', repos = repos);
            library({packages})
        "),
        collapse=";\n"
    )
    
    print(statement)

    eval(parse(text=statement))
}