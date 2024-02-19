#' @name easy_gsea
#' @rdname easy_gsea
#' @title A simple gsea workflow following DESeq or limma results
#'
#' @aliases easy_gsea
#'
#' @import fgsea
#' @export

easy_gsea = function(res, pathways) {

  # Test if input dataframe is from DESeq or limma
  if ('stat' %in% colnames(res)) {
    res2 <- data.frame(symbol=rownames(res), res$stat)
  } else if ('t' %in% colnames(res)) {
    res2 <- data.frame(symbol=rownames(res), res$t)
  }

  # Continue with fgsea workflow
  colnames(res2) <- c('SYMBOL', 'stat')
  res2 <- res2[complete.cases(res2),]

  res2 <- res2[order(res2$stat, decreasing = T),]

  ranks <- deframe(res2)

  fgseaRes <- fgsea(pathways=pathways, stats=ranks, nproc=1)

  return(fgseaRes)
}
