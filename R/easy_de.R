#' @name easy_de
#' @rdname easy_de
#' @title Simple differential gene expression analysis with DESeq2 and limma
#'
#' @description
#' DESeq method requires whole number raw counts, limma seq requires normally distributed data.
#'
#' @aliases easy_de
#'
#' @import dplyr
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom limma lmFit makeContrasts contrasts.fit eBayes topTable
#' @export

easy_de = function(df, group, method='limma') {

  if (substr(tolower(method), 1, 5) == 'deseq') {

    # Check if input is whole number

    if (sum(round(df) != df) == 0) stop('Input has to raw counts when using method "deseq"')

    # Standard DESeq procedure

    df1 = df[,colnames(df) %in% group]
    df2 = df[,!(colnames(df) %in% group)]

    df = cbind(df1, df2)

    condition <- factor(c(rep('select',ncol(df1)),rep('other',ncol(df2))))
    colData <- data.frame(row.names=colnames(df), condition)

    dds <- DESeqDataSetFromMatrix(countData = round(df), colData = colData, design = ~ condition)
    dds1 <- DESeq(dds, fitType = 'mean', minReplicatesForReplace = 7, parallel = FALSE)

    res <- results(dds1)

  } else if (substr(tolower(method), 1, 5) == 'limma') {

    vec = colnames(df)
    for (i in c(1:length(vec))) if (vec[i] %in% group) vec[i]='select' else vec[i]='other'

    list <- vec %>% factor(., levels = c('select', 'other'), ordered = F)

    list <- model.matrix(~factor(list)+0)  # Set group as model matrix

    colnames(list) <- c('select', 'other')

    df.fit <- lmFit(df, list)
    df.matrix <- makeContrasts(select - other, levels = list)

    fit <- contrasts.fit(df.fit, df.matrix)
    fit <- eBayes(fit)
    res <- topTable(fit, coef=1, n = Inf)

  } else {

    stop('Method has to be one of "limma" or "deseq"')

  }

  return(as.data.frame(res))
}
