source("src/installer.R")
install_over("xtable", "ellipse", "RColorBrewer")
is_outlier <- function(x, coef = 1.5){
  res <- x
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  .IQR <- IQR(x, na.rm = TRUE)
  upper.limit <- Q3 + (coef*.IQR)
  lower.limit <- Q1 - (coef*.IQR)
  return(x < lower.limit | x > upper.limit)
}

textbl <- function(..., include.rownames=FALSE) {
    print(
        xtable::xtable(...),
    include.rownames=include.rownames
)}


corrplot <- function(cor_matrix, save_filename) {
    my_colors <- brewer.pal(11, "RdBu")
    my_colors <- colorRampPalette(my_colors)(100)
    # Criar o gráfico de correlação
    ord <- order(cor_matrix[1, ])
    data_ord <- cor_matrix[ord, ord]
    pdf(save_filename)
    ellipse::plotcorr(
      cor_matrix,
      mar=c(1,1,1,1),
      col=my_colors[cor_matrix*50+50],
      type = "lower"
    )
    dev.off()
}
