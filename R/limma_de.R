#' @name limma_de
#' @rdname limma_de
#' @title Differentially expressed gene analysis using limma
#'
#' @aliases limma_de
#'
#' @import dplyr
#' @importFrom limma lmFit makeContrasts contrasts.fit eBayes topTable
#' @export

limma_de = function(df, group) {

  vec = colnames(df)
  for (i in c(1:length(vec))) if (vec[i] %in% group) vec[i]='select' else vec[i]='other'

  list <- vec %>% factor(., levels = c('select', 'other'), ordered = F)

  list <- model.matrix(~factor(list)+0)  # Set group as model matrix

  colnames(list) <- c('select', 'other')

  df.fit <- lmFit(df, list)
  df.matrix <- makeContrasts(select - other, levels = list)

  fit <- contrasts.fit(df.fit, df.matrix)
  fit <- eBayes(fit)
  tempOutput <- topTable(fit, coef=1, n = Inf)

  return(tempOutput)
}
