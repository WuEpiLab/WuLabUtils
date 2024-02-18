#' @name deseq_de
#' @rdname deseq_de
#' @title Differentially expressed gene analysis using DESeq2
#'
#' @aliases deseq_de
#'
#' @import DESeq2
#' @export

deseq_de = function(df, group) {
  library(DESeq2)

  df1 = df[,colnames(df) %in% group]
  df2 = df[,!(colnames(df) %in% group)]

  df = cbind(df1, df2)

  condition <- factor(c(rep('select',ncol(df1)),rep('other',ncol(df2))))
  colData <- data.frame(row.names=colnames(df), condition)

  dds <- DESeqDataSetFromMatrix(countData = round(df), colData = colData, design = ~ condition)
  dds1 <- DESeq(dds, fitType = 'mean', minReplicatesForReplace = 7, parallel = FALSE)

  res <- results(dds1)

  return(as.data.frame(res))
}
